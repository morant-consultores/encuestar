if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ":=", "distinto", "cluster_0"))
}

#' Title
#'
#' @param respuestas
#' @param shp
#' @param mantener
#'
#' @return
#' @export
#'
#' @examples

corregir_cluster <- function(respuestas, shp, mantener, nivel, var_n) {
  enc_shp <- respuestas %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = "+init=epsg:4326")

  todas <- enc_shp %>%
    st_join(shp %>% filter(sf::st_geometry_type(.) != "POINT"))

  if (length(mantener) != 0) {
    fuera <- todas %>%
      filter(
        is.na(!!rlang::sym(glue::glue("{var_n}.y"))),
        !(!!rlang::sym(glue::glue("{var_n}.x")) %in% mantener)
      )
    fuera_sm <- todas %>%
      filter(
        is.na(!!rlang::sym(glue::glue("{var_n}.y"))),
        !!rlang::sym(glue::glue("{var_n}.x")) %in% mantener
      )
  } else {
    fuera <- todas %>% filter(is.na(!!rlang::sym(glue::glue("{var_n}.y"))))
  }

  ja <- st_distance(fuera, shp) %>%
    as_tibble %>%
    rowwise() %>%
    mutate(id = which.min(c_across(everything())))
  fuera <- fuera %>%
    mutate(!!rlang::sym(nivel) := as.character(shp[[nivel]][ja$id]))

  if (length(mantener) != 0) {
    fuera_sm <- fuera_sm %>%
      mutate(
        !!rlang::sym(nivel) := as.character(
          !!rlang::sym(glue::glue("{var_n}.y"))
        )
      )
    fuera <- bind_rows(fuera, fuera_sm)
  }
  fuera <- fuera %>%
    mutate(
      distinto = !!rlang::sym(glue::glue("{nivel}")) !=
        !!rlang::sym(glue::glue("{var_n}.x"))
    )

  dentro <- todas %>%
    filter(!is.na(!!rlang::sym(glue::glue("{var_n}.y")))) %>%
    mutate(
      distinto = !!rlang::sym(nivel) != !!rlang::sym(glue::glue("{var_n}.x")),
      !!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))
    )

  nuevos <- dentro %>%
    bind_rows(fuera) %>%
    as_tibble() %>%
    filter(distinto) %>%
    select(cluster_0, !!rlang::sym(nivel), distinto)

  respuestas <- left_join(respuestas, nuevos, by = "cluster_0")

  respuestas$distinto[is.na(respuestas$distinto)] <- F

  respuestas <- respuestas %>%
    mutate(
      !!rlang::sym(glue::glue("{var_n}")) := if_else(
        distinto,
        !!rlang::sym(nivel),
        !!rlang::sym(glue::glue("{var_n}"))
      )
    ) %>%
    select(-!!rlang::sym(nivel), -distinto)

  print(glue::glue(
    "Se cambiaron {nuevos %>% count(distinto) %>% pull(n)} clusters ya que la entrevista no esta donde se reporto."
  ))
  return(respuestas)
}


#' Title
#'
#' @param base_sf
#' @param var_n
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
dist_poligonos <- function(base_sf, shp, var_n, nivel) {
  base_sf[[var_n]] %>%
    unique() %>%
    map_df(
      ~ {
        respuestas <- base_sf %>% filter(!!sym(var_n) == .x)
        sec <- shp %>% filter(!!rlang::sym(nivel) == .x)
        dist <- st_distance(respuestas, sec) %>%
          as_tibble() %>%
          set_names("distancia")
        respuestas %>% bind_cols(dist)
      }
    ) %>%
    as_tibble %>%
    select(-geometry)
}

#' Title
#'
#' @param base_sf
#' @param encuesta
#' @param muestra
#' @param var_n
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
dist_puntos <- function(base_sf, encuesta, muestra, var_n, nivel) {
  loc <- encuesta$shp_completo$shp[[length(encuesta$shp_completo$shp)]] %>%
    filter(st_geometry_type(.) == "POINT") %>%
    inner_join(
      muestra$muestra[[length(muestra$muestra)]] %>% tidyr::unnest(data)
    )

  unique(loc[[nivel]]) %>%
    map_df(
      ~ {
        respuestas <- base_sf %>% filter(!!rlang::sym(var_n) == .x)
        locs <- loc %>% filter(!!rlang::sym(nivel) == .x)
        respuestas %>%
          bind_cols(
            st_distance(respuestas, locs) %>%
              as_tibble %>%
              rowwise() %>%
              mutate(distancia = min(c_across(everything()))) %>%
              select(distancia)
          ) %>%
          as_tibble %>%
          select(-geometry)
      }
    )
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "SbjNum",
    "INT15",
    "T_Q_47_1",
    "rowname.x",
    "rowname.y",
    "bd"
  ))
}

#' Title
#'
#' @param encuesta_qro
#'
#' @return
#' @export
#'
#' @examples

match_dicc_base <- function(self) {
  self$respuestas$base |>
    names() |>
    as_tibble() |>
    anti_join(
      self$respuestas$catalogo |>
        pull(variable) |>
        as_tibble(),
      by = "value"
    ) |>
    rename(bd = value) |>
    transmute(variable = bd) |>
    filter(
      !variable %in%
        (catalogo_variables |>
          filter(plataforma == "encuestar") |>
          pull(variable))
    )
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("nivel", "llave", "Municipio", "Localidad"))
}

#' Title
#'
#' @param self
#'
#' @return
#' @export
#'
#' @examples

var_clave_diccionario <- function(self, diseno) {
  cluster_id <- diseno$niveles %>%
    filter(nivel == diseno$ultimo_nivel) %>%
    pull(llave)
  post_id <- diseno$cuotas %>%
    select(-Municipio, -Localidad, -contains("cluster"), -n) %>%
    names

  filtros <- self$cuestionario$documento %>%
    officer::docx_summary() %>%
    as_tibble %>%
    filter(
      style_name == "Morant_filtros" | style_name == "Preguntas_filtros",
      stringr::str_detect(text, "\\{")
    ) %>%
    transmute(
      text = stringr::str_extract(text, "(?<=\\{).+?(?=\\})") %>%
        stringr::str_squish()
    ) %>%
    pull(1)

  if_else(
    !cluster_id %in% filtros,
    glue::glue("La variable {cluster_id} no se encuentra en el cuestionario"),
    ""
  )
  if_else(
    !all(post_id %in% filtros),
    glue::glue(
      "Las variables {paste(post_id, collapse = ', ')} no se encuentran en el cuestionario"
    ),
    ""
  )
}
