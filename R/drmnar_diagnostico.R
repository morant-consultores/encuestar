# Diagnóstico de no respuesta no ignorable por pregunta y subconjunto ----
#
# Implementa la regla de decisión operativa de la Sección 5 del Anexo
# Técnico DR-MNAR: se estima el parámetro crítico gamma_y con el
# instrumento aleatorizado y se prueba H0: gamma_y = 0 (ignorabilidad).
#   - IC 95% excluye el 0  => no respuesta NO ignorable => activar DR-MNAR
#   - IC 95% incluye el 0  => MAR => conservar Raking (menor varianza)

#' Diagnóstico de no respuesta no ignorable por pregunta y subconjunto
#'
#' Estima el parámetro crítico de no ignorabilidad `gamma_y` (vía el
#' estimador doblemente robusto de [estimar_drmnar()]) para cada pregunta
#' y subconjunto solicitado, y aplica la regla de decisión DR-MNAR vs
#' Raking de la Sección 5 del Anexo Técnico.
#'
#' El diagnóstico por subconjunto importa porque el sesgo puede cancelarse
#' globalmente y ser severo dentro de un subgrupo (o tener signos opuestos
#' entre subgrupos).
#'
#' @param diseno Objeto `survey::svydesign` del flujo de encuestar.
#' @param preguntas Preguntas a diagnosticar. Puede ser un vector de
#'   nombres (variables ya dicotómicas 0/1) o una lista nombrada
#'   `list(pregunta = categoria(s))`; con varias categorías se produce una
#'   estimación por categoría (una-vs-resto).
#' @param covariables Vector de covariables para los modelos de propensión
#'   e imputación (demográficas + contextuales por sección).
#' @param instrumento Columna con el brazo del instrumento aleatorizado.
#' @param respuesta_ind Columna con el indicador de respuesta del ítem;
#'   `NULL` lo deriva de los NA de la pregunta.
#' @param subconjuntos Lista nombrada de vectores lógicos (`NULL` en un
#'   elemento = todas las observaciones). Default: un solo grupo "Estado".
#' @param nivel_confianza Nivel del intervalo para la prueba de
#'   ignorabilidad (default 95%).
#' @param ... Argumentos adicionales para [estimar_drmnar()].
#' @return Tibble con una fila por pregunta x categoría x subconjunto:
#'   `pregunta`, `categoria`, `subconjunto`, `gamma_y`, `ee`, `inf`, `sup`,
#'   `z_stat`, `no_ignorable`, `decision` ("DR-MNAR", "Raking" o
#'   "Sin estimación"), `n`, `convergencia`, `est_drmnar`, `est_rake`,
#'   `diferencia` (cuánto se modifica la estimación al corregir por MNAR).
#' @examples
#' \dontrun{
#' diagnosticar_norespuesta(
#'   diseno = encuesta$muestra$diseno,
#'   preguntas = list(
#'     "conoce_cruz" = "Sí lo conoce",
#'     "conoce_andrea" = "Sí lo conoce"
#'   ),
#'   covariables = c("sexo", "rango_edad"),
#'   subconjuntos = list(
#'     "Estado" = NULL,
#'     "Juárez" = datos$municipio == "JUAREZ"
#'   )
#' )
#' }
#' @export
diagnosticar_norespuesta <- function(diseno, preguntas, covariables = NULL,
                                     instrumento = "drmnar_z",
                                     respuesta_ind = NULL,
                                     subconjuntos = NULL,
                                     nivel_confianza = 0.95,
                                     ...) {
  if (is.null(subconjuntos)) {
    subconjuntos <- list("Estado" = NULL)
  }
  if (is.null(names(subconjuntos)) || any(names(subconjuntos) == "")) {
    stop("`subconjuntos` debe ser una lista nombrada.")
  }

  # normalizar preguntas a lista nombrada pregunta -> categorías (NULL = ya 0/1)
  if (!is.list(preguntas)) {
    preguntas <- stats::setNames(
      vector("list", length(preguntas)),
      preguntas
    )
  }

  filas <- list()
  for (nombre_sub in names(subconjuntos)) {
    filtro <- subconjuntos[[nombre_sub]]
    for (preg in names(preguntas)) {
      categorias <- preguntas[[preg]]
      if (is.null(categorias)) {
        categorias <- list(NULL)
      }
      for (cat_i in categorias) {
        res <- tryCatch(
          estimar_drmnar(
            diseno = diseno, pregunta = preg, covariables = covariables,
            instrumento = instrumento, respuesta_ind = respuesta_ind,
            categoria = cat_i, subconjunto = filtro,
            nombre_subconjunto = nombre_sub,
            nivel_confianza = nivel_confianza,
            ...
          ),
          error = function(e) {
            warning(
              "No se pudo estimar `", preg, "` en `", nombre_sub, "`: ",
              conditionMessage(e),
              call. = FALSE
            )
            NULL
          }
        )

        if (is.null(res)) {
          filas[[length(filas) + 1]] <- tibble::tibble(
            pregunta = preg,
            categoria = if (is.null(cat_i)) NA_character_ else
              paste(cat_i, collapse = " | "),
            subconjunto = nombre_sub,
            gamma_y = NA_real_, ee = NA_real_,
            inf = NA_real_, sup = NA_real_, z_stat = NA_real_,
            no_ignorable = NA,
            decision = "Sin estimación",
            n = NA_integer_, convergencia = FALSE,
            est_drmnar = NA_real_, est_rake = NA_real_,
            diferencia = NA_real_
          )
          next
        }

        dr <- res[res$modelo == "DR-MNAR", ]
        rake <- res[res$modelo == "Weights-MAR", ]
        gamma <- dr$gamma_y
        ee_gamma <- dr$ee_gamma_y
        z_crit <- stats::qnorm(1 - (1 - nivel_confianza) / 2)
        inf <- gamma - z_crit * ee_gamma
        sup <- gamma + z_crit * ee_gamma
        estimable <- !is.na(gamma) && !is.na(ee_gamma) && dr$convergencia
        no_ign <- if (estimable) (inf > 0 || sup < 0) else NA

        filas[[length(filas) + 1]] <- tibble::tibble(
          pregunta = preg,
          categoria = dr$categoria,
          subconjunto = nombre_sub,
          gamma_y = gamma, ee = ee_gamma,
          inf = inf, sup = sup,
          z_stat = gamma / ee_gamma,
          no_ignorable = no_ign,
          decision = dplyr::case_when(
            !estimable ~ "Sin estimación",
            no_ign ~ "DR-MNAR",
            .default = "Raking"
          ),
          n = dr$n, convergencia = dr$convergencia,
          est_drmnar = dr$est,
          est_rake = rake$est,
          diferencia = dr$est - rake$est
        )
      }
    }
  }

  dplyr::bind_rows(filas)
}

#' Resumen de la decisión metodológica por pregunta
#'
#' Agrega el diagnóstico de [diagnosticar_norespuesta()] a nivel pregunta:
#' si alguna categoría o subconjunto presenta no respuesta no ignorable,
#' la pregunta se reporta con el método DR-MNAR; en caso contrario se
#' conserva el Raking tradicional (raking y DR-MNAR son mutuamente
#' excluyentes por estimando).
#'
#' @param diagnostico Tibble producido por [diagnosticar_norespuesta()].
#' @return Tibble con una fila por pregunta: `pregunta`, `n_estimaciones`,
#'   `n_no_ignorables`, `gamma_max_abs`, `decision`.
#' @export
resumen_decision_norespuesta <- function(diagnostico) {
  diagnostico |>
    dplyr::group_by(pregunta) |>
    dplyr::summarise(
      n_estimaciones = dplyr::n(),
      n_no_ignorables = sum(no_ignorable, na.rm = TRUE),
      gamma_max_abs = ifelse(
        all(is.na(gamma_y)),
        NA_real_,
        max(abs(gamma_y), na.rm = TRUE)
      ),
      decision = ifelse(
        any(no_ignorable, na.rm = TRUE), "DR-MNAR", "Raking"
      ),
      .groups = "drop"
    )
}
