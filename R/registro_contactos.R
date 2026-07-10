# Registro de contactos por intento -------------------------------------
#
# Acuerdo metodológico con campo: cada renglón del snapshot acumula TODOS
# sus intentos de entrevista (tocó y no abrió, rechazo, efectiva, ...)
# hasta un máximo de 15, en columnas INT_1..INT_15 (según la ola, el
# esquema puede venir sin guión: INT1..INT15). El audio solo se graba
# mientras se contesta el cuestionario (el renglón), así que auditoría
# usa este registro para verificar que el encuestador siguió la
# metodología entre grabación y grabación.
#
# Estado en Chihuahua (verificado 2026-07): feb-2026 trae el esquema
# INT1..INT15 pero VACÍO (la app aún no registraba el detalle);
# sep-2025 ni siquiera trae el esquema. Ambas olas sí traen `INT` (el
# número del intento en que se logró la entrevista, topado en 15), que
# sirve de respaldo para contar contactos mientras el detalle empieza a
# fluir en la próxima ola.

.detectar_columnas_intentos <- function(bd, prefijo = NULL,
                                        max_intentos = 15) {
  candidatos <- if (is.null(prefijo)) c("INT_", "INT") else prefijo
  for (p in candidatos) {
    cols <- paste0(p, seq_len(max_intentos))
    cols <- cols[cols %in% names(bd)]
    if (length(cols) > 0) {
      return(cols)
    }
  }
  character(0)
}

.normalizar_resultado <- function(x) {
  # chartr y no iconv//TRANSLIT: en macOS TRANSLIT vuelve "ó" -> "'o" y el
  # nombre de columna cambiaría por plataforma
  x <- chartr("áéíóúÁÉÍÓÚñÑüÜ",
              "aeiouaeiounnuu", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

#' Pivotear los intentos de entrevista de ancho a largo
#'
#' Convierte las columnas de intentos del snapshot (`INT_1..INT_15` o
#' `INT1..INT15`, una por toque de puerta hasta el máximo acordado de 15)
#' a formato largo: una fila por intento con su número y su resultado
#' (tocó y no abrió, rechazo, efectiva, ...). Es el insumo de auditoría
#' (verificar la metodología del encuestador contra la grabación, que
#' solo existe para el cuestionario) y de [derivar_registro_contactos()].
#'
#' @param bd Snapshot de entrevistas (una fila por renglón levantado).
#' @param id Columna que identifica el renglón (default `"SbjNum"`).
#' @param seccion Columna de la sección (default `"SECCION"`).
#' @param prefijo Prefijo de las columnas de intentos. `NULL` (default)
#'   autodetecta: primero `INT_`, luego `INT`.
#' @param max_intentos Máximo de intentos registrables (default 15, el
#'   acuerdo con campo).
#' @param codigos Catálogo opcional para recodificar resultados: vector
#'   nombrado `c("1" = "Efectiva", "2" = "No abrió", ...)`. Los códigos
#'   fuera del catálogo se conservan crudos y se avisa (no se pierde
#'   información).
#'
#' @return `tibble` con una fila por intento registrado: las columnas
#'   `id` y `seccion`, `intento` (1..`max_intentos`) y `resultado`.
#'   Si las columnas existen pero vienen vacías (caso Chihuahua
#'   feb-2026), devuelve 0 filas con un aviso.
#' @export
pivotar_intentos <- function(bd, id = "SbjNum", seccion = "SECCION",
                             prefijo = NULL, max_intentos = 15,
                             codigos = NULL) {
  for (col in c(id, seccion)) {
    if (!col %in% names(bd)) {
      stop("No existe la columna `", col, "` en el snapshot.", call. = FALSE)
    }
  }
  cols <- .detectar_columnas_intentos(bd, prefijo, max_intentos)
  if (length(cols) == 0) {
    stop(
      "No hay columnas de intentos en el snapshot (se buscaron INT_1..INT_",
      max_intentos, " e INT1..INT", max_intentos, "). Este es el registro ",
      "acordado con campo; si la ola es previa al acuerdo, usa ",
      "derivar_registro_contactos(), que cae a la columna `INT`.",
      call. = FALSE
    )
  }

  largo <- bd |>
    dplyr::select(dplyr::all_of(c(id, seccion, cols))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character)) |>
    tidyr::pivot_longer(
      dplyr::all_of(cols),
      names_to = "intento", values_to = "resultado"
    ) |>
    dplyr::mutate(
      intento = as.integer(sub("^INT_?", "", intento))
    ) |>
    dplyr::filter(!is.na(resultado), resultado != "") |>
    dplyr::arrange(.data[[id]], intento)

  if (nrow(largo) == 0) {
    warning(
      "Las columnas de intentos (", cols[1], "...) existen pero vienen ",
      "vacías: la app aún no registra el detalle por intento en esta ola.",
      call. = FALSE
    )
    return(largo)
  }

  if (!is.null(codigos)) {
    conocidos <- largo$resultado %in% names(codigos)
    if (any(!conocidos)) {
      warning(
        "Código(s) de intento fuera del catálogo (quedan crudos): ",
        paste(unique(largo$resultado[!conocidos]), collapse = ", "),
        call. = FALSE
      )
    }
    largo$resultado[conocidos] <- unname(codigos[largo$resultado[conocidos]])
  }

  largo
}

#' Derivar el registro de contactos por sección (insumo de la sobremuestra)
#'
#' Agrega los intentos de entrevista a la tabla por sección que consume
#' `muestreaR::planear_siguiente_ola()`: `seccion`, `contactos` (todos
#' los toques de puerta registrados), `efectivas` (renglones levantados)
#' y el desglose por resultado (`intentos_no_abrio`, `intentos_rechazo`,
#' ...). Con ella la sobremuestra de la ola siguiente se dimensiona por
#' la tasa de respuesta REAL de cada sección.
#'
#' Dos fuentes, en orden de preferencia:
#' \describe{
#'   \item{detalle por intento}{columnas `INT_1..INT_15` / `INT1..INT15`
#'     (vía [pivotar_intentos()]): contactos y desglose completos.}
#'   \item{respaldo `INT`}{cuando el detalle no existe o viene vacío
#'     (olas previas al acuerdo, p. ej. Chihuahua sep-2025 y feb-2026):
#'     `INT` es el número del intento en que se logró la entrevista, así
#'     que `contactos = sum(INT)` con `INT` faltante contado como 1. Se
#'     avisa la limitación: sin tipos de fallo y sin las viviendas donde
#'     nunca se logró entrevista.}
#' }
#'
#' @inheritParams pivotar_intentos
#' @param col_intento_efectivo Columna de respaldo con el número del
#'   intento efectivo (default `"INT"`).
#'
#' @return `tibble` con una fila por sección: `seccion` (caracter),
#'   `contactos`, `efectivas` y, con detalle, una columna
#'   `intentos_<resultado>` por cada resultado observado. Atributo
#'   `"fuente"`: `"detalle_intentos"` o `"intento_efectivo"`.
#' @export
derivar_registro_contactos <- function(bd, id = "SbjNum",
                                       seccion = "SECCION",
                                       prefijo = NULL, max_intentos = 15,
                                       codigos = NULL,
                                       col_intento_efectivo = "INT") {
  if (!seccion %in% names(bd)) {
    stop("No existe la columna `", seccion, "` en el snapshot.",
         call. = FALSE)
  }

  cols <- .detectar_columnas_intentos(bd, prefijo, max_intentos)
  largo <- if (length(cols) > 0) {
    suppressWarnings(
      pivotar_intentos(bd, id = id, seccion = seccion, prefijo = prefijo,
                       max_intentos = max_intentos, codigos = codigos)
    )
  } else {
    NULL
  }

  efectivas <- bd |>
    dplyr::mutate(seccion = as.character(.data[[seccion]])) |>
    dplyr::count(seccion, name = "efectivas")

  if (!is.null(largo) && nrow(largo) > 0) {
    # ---- fuente completa: el detalle por intento ----
    conteos <- largo |>
      dplyr::mutate(seccion = as.character(.data[[seccion]])) |>
      dplyr::count(seccion, resultado)
    registro <- conteos |>
      dplyr::count(seccion, wt = n, name = "contactos") |>
      dplyr::left_join(efectivas, by = "seccion") |>
      dplyr::left_join(
        conteos |>
          dplyr::mutate(
            resultado = paste0("intentos_", .normalizar_resultado(resultado))
          ) |>
          tidyr::pivot_wider(names_from = resultado, values_from = n,
                             values_fill = 0),
        by = "seccion"
      )
    attr(registro, "fuente") <- "detalle_intentos"
    return(registro)
  }

  # ---- respaldo: la columna INT (número del intento efectivo) ----
  if (!col_intento_efectivo %in% names(bd)) {
    stop(
      "Sin detalle de intentos (INT_1..INT_", max_intentos, ") ni columna ",
      "de respaldo `", col_intento_efectivo, "`: no se puede derivar el ",
      "registro de contactos.",
      call. = FALSE
    )
  }
  warning(
    "Sin detalle por intento en esta ola: contactos derivados de `",
    col_intento_efectivo, "` (número del intento efectivo). Limitación: ",
    "no distingue tipos de fallo (no abrió / rechazo) ni cuenta las ",
    "viviendas donde nunca se logró entrevista.",
    call. = FALSE
  )
  registro <- bd |>
    dplyr::mutate(
      seccion = as.character(.data[[seccion]]),
      # una entrevista con INT faltante existió: mínimo 1 intento
      .int = pmax(dplyr::coalesce(.data[[col_intento_efectivo]], 1L), 1L)
    ) |>
    dplyr::group_by(seccion) |>
    dplyr::summarise(
      contactos = sum(.int),
      efectivas = dplyr::n(),
      .groups = "drop"
    )
  attr(registro, "fuente") <- "intento_efectivo"
  registro
}
