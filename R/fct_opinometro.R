#' Title
#'
#' @param pool
#' @param id_cuestionario
#'
#' @return
#'
#' @examples
determinar_contenidoCuestionario <- function(pool, id_cuestionario) {
  tbl(src = pool, "Encuesta") %>%
    filter(Id == id_cuestionario) %>%
    collect() %>%
    mutate(
      pages = map(
        JsonData,
        ~ jsonlite::fromJSON(.x, simplifyDataFrame = FALSE)$pages
      )
    ) %>%
    select(-JsonData) %>%
    unnest_longer(col = pages, values_to = "page") %>%
    unnest_wider(col = page) %>%
    # 1) Despliega la lista de elementos en filas
    unnest_longer(col = elements) %>%
    # 2) Descompón cada elemento (que es a su vez una lista) en columnas,
    #    usando names_sep si quieres prefijar los nombres
    unnest_wider(col = elements, names_sep = "")
}
#' Title
#'
#' @param pool
#' @param codigos
#' @param encuesta_id
#'
#' @return
#'
#' @examples
consultar_respuestas <- function(pool, codigos, encuesta_id) {
  query_claves <- paste0(
    "REPLACE(JSON_VALUE(r.Resultado, '$.",
    codigos,
    "'), 'ñ', 'n') AS ",
    codigos,
    collapse = ", "
  )
  encuesta_id <- encuesta_id |> toString()

  query <- glue::glue(
    "
  SELECT
    r.Id,
    r.EncuestaId,
    r.FechaInicio,
    r.FechaFin,
    r.FechaCreada,
    r.UbicacionAplicada,
    r.UsuarioNum,
    r.isComplete,
    ",
    query_claves,
    "
  FROM
    Registros r
  WHERE EncuestaId in ({encuesta_id})
"
  )

  dplyr::tbl(pool, dplyr::sql(query)) |>
    left_join(
      tbl(pool, "Usuarios") |>
        semi_join(
          tbl(pool, "UsuariosEncuesta") |>
            filter(EncuestaId == encuesta_id),
          join_by(Id == UsuarioId)
        ) |>
        select(UsuarioNum = Num, Nombre, APaterno, AMaterno),
      by = "UsuarioNum"
    ) |>
    collect()
}
#' Consulta y aplana las respuestas existentes de un cuestionario
#'
#' @description Obtiene todos los registros del cuestionario indicado desde la
#'   base de datos, aplana el JSON de respuestas en columnas individuales y
#'   adjunta el nombre del encuestador.
#'
#' @param pool Objeto `pool` con la conexión a la base de datos.
#' @param id_cuestionario Entero. Identificador del cuestionario en Opinómetro.
#'
#' @return Un [tibble()] con una fila por registro y una columna por pregunta.
#' @export
consultar_respuestas_existentes <- function(pool, id_cuestionario) {
  respuestas_enLista <-
    tbl(src = pool, "Registros") |>
    filter(EncuestaId == id_cuestionario) |>
    collect() %>%
    purrr::pmap_df(function(
      Id,
      EncuestaId,
      FechaInicio,
      FechaFin,
      FechaCreada,
      UbicacionAplicada,
      UsuarioNum,
      TipoRegistro,
      Resultado,
      ...
    ) {
      list_respuestas <- list()

      list_respuestas$Id = Id
      list_respuestas$EncuestaId = EncuestaId
      list_respuestas$FechaInicio = FechaInicio
      list_respuestas$FechaFin = FechaFin
      list_respuestas$FechaCreada = FechaCreada
      list_respuestas$UbicacionAplicada = UbicacionAplicada
      list_respuestas$UsuarioNum = UsuarioNum
      list_respuestas$TipoRegistro = TipoRegistro

      list_respuestas <-
        list_respuestas |>
        append(
          Resultado |>
            jsonlite::fromJSON()
        )

      # Aplanar las listas de más de un elemento a varias listas de un solo elemento
      bd_respuestas <- do.call(
        c,
        lapply(seq_along(list_respuestas), function(i) {
          nombre <- names(list_respuestas)[i]
          valores <- list_respuestas[[i]]
          if (length(valores) > 1) {
            setNames(as.list(valores), paste0(nombre, "_O", seq_along(valores)))
          } else {
            setNames(list(valores), nombre)
          }
        })
      )
      return(bd_respuestas)
    }) |>
    relocate(Id) |>
    left_join(
      tbl(pool, "Usuarios") |>
        semi_join(
          tbl(pool, "UsuariosEncuesta") |>
            filter(EncuestaId == id_cuestionario),
          join_by(Id == UsuarioId)
        ) |>
        select(UsuarioNum = Num, Nombre, APaterno, AMaterno) |>
        collect(),
      by = "UsuarioNum"
    ) |>
    relocate(Nombre, .after = UsuarioNum) |>
    relocate(APaterno, .after = Nombre) |>
    relocate(AMaterno, .after = APaterno)
}
#' rectificar_respuestasOpinometro
#'
#' @param bd_respuestasOpinometro
#' @param variables_cuestionario
#'
#' @return
#'
#' @examples
rectificar_respuestasOpinometro <- function(
  bd_respuestas_raw,
  variables_cuestionario,
  var_ubicacion = "gps",
  elim_na_ub = T
) {
  bd_respuestas_raw %>%
    mutate(gps_aux = !!rlang::sym(var_ubicacion)) %>%
    # mutate(intentos = stringr::str_trim(string = intentos, side = "both")) |>
    # filter(intentos == "Abrieron la puerta, aceptaron la entrevista y cumple el perfil") |>
    {
      if (elim_na_ub) filter(., gps_aux != "No aplica") else .
    } %>%
    {
      if (elim_na_ub) filter(., !is.na(gps_aux)) else .
    } %>%
    # mutate(UbicacionAplicada = dplyr::if_else(condition = UbicacionAplicada == ",",
    #                                            true = NA_character_,
    #                                            false = UbicacionAplicada)) |>
    tidyr::separate(
      col = gps_aux,
      into = c("Latitude", "Longitude"),
      sep = ",",
      remove = TRUE
    ) |>
    transmute(
      SbjNum = Id,
      Date = lubridate::as_datetime(FechaInicio),
      Srvyr = paste(Nombre, APaterno, AMaterno, sep = " "),
      UsuarioNum = UsuarioNum,
      Srvyr_Nombre = Nombre,
      Srvyr_APaterno = APaterno,
      Srvyr_AMaterno = AMaterno,
      FechaInicio = FechaInicio,
      VStart = lubridate::as_datetime(FechaInicio),
      VEnd = lubridate::as_datetime(FechaFin),
      Duration = as.character(difftime(VEnd, VStart, units = "hours")),
      TipoRegistro,
      Latitude,
      Longitude,
      across(all_of(variables_cuestionario)),
      #intentos = TipoRegistro_aux,
      corte = update(
        Sys.time(),
        minute = floor(lubridate::minute(Sys.time()) / 15) * 15,
        second = 0,
        tz = "America/Mexico_City"
      ),
      SECCION = as.character(as.numeric(cluster))
    ) # |>  filter(Date <= corte)
}
#' Title
#'
#' @param bd_respuestasOpinometro
#'
#' @return
#'
#' @examples
calcular_intentosEfectivos_opinometro <- function(bd_respuestasOpinometro) {
  bd_respuestasOpinometro |>
    transmute(
      SbjNum = Id,
      Srvyr = paste(Nombre, APaterno, AMaterno, sep = " "),
      intentos = stringr::str_trim(string = TipoRegistro, side = "both")
    ) |>
    mutate(
      intento_efectivo = dplyr::case_when(
        intentos == "Efectivo" ~ "efectivo",
        .default = "no efectivo"
      )
    ) |>
    mutate(flag = cumsum(intento_efectivo == "efectivo")) %>%
    group_by(flag) %>%
    mutate(intento_efectivo = n()) %>%
    ungroup() %>%
    select(SbjNum, intento_efectivo)
}
