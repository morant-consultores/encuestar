# Covariables contextuales por sección electoral (DR-MNAR) ---------------
#
# Fusión de datos de la Sección 4 del Anexo Técnico: indicadores censales
# de INEGI (escolaridad promedio, conectividad, urbanización) e histórico
# electoral por sección (participación promedio y Margen de Victoria Neto)
# que alimentan los modelos de propensión e imputación del DR-MNAR.
#
# Formatos de entrada:
# - censo_seccion: censo 2020 por sección (inegi/seccion_2020.rda), llave
#   `seccion` = "EE_SSSS" (p. ej. "08_0001"), con `graproes`, `vph_inter`,
#   `vivpar_hab` y `tipo`.
# - electoral_bd: histórico electoral estilo aelectoral (mex.rda$info$bd),
#   llave `seccion` = "EE_SSSS" y columnas `ele_<partido(s)>_<eleccion>`.

#' Partidos que componen una columna electoral
#' @keywords internal
#' @noRd
partes_columna_electoral <- function(columna, eleccion) {
  sin_prefijo <- sub("^ele_", "", columna)
  sin_eleccion <- sub(paste0("_", eleccion, "$"), "", sin_prefijo)
  strsplit(sin_eleccion, "_", fixed = TRUE)[[1]]
}

#' Margen de Victoria Neto histórico por sección electoral
#'
#' Implementa la definición operativa del Anexo Técnico (Sección 4): la
#' diferencia neta de votación promedio de la sección entre el bloque de
#' referencia oficialista (MORENA + PVEM + PT) y el bloque opositor
#' consolidado (PAN + PRI + PRD + MC), normalizada por la votación total y
#' promediada sobre las elecciones indicadas. Las columnas de coalición
#' (p. ej. `ele_pt_morena_pr_24`) se asignan al bloque cuyo conjunto de
#' partidos las contiene; votos nulos, no registrados e independientes no
#' cuentan en ningún bloque.
#'
#' Un margen positivo alto indica hegemonía oficialista; negativo alto, un
#' bastión opositor; cercano a cero, territorio competitivo. Esta variable
#' unificada reemplaza los porcentajes por partido para evitar la
#' multicolinealidad severa en el modelo de imputación.
#'
#' @param electoral_bd Base electoral por sección con columnas
#'   `ele_<partido(s)>_<eleccion>` (formato de `aelectoral`).
#' @param elecciones Vector de sufijos de elección a promediar
#'   (p. ej. `c("pr_24", "gb_23", "pr_18")`).
#' @param bloque_oficialista Partidos del bloque de referencia.
#' @param bloque_opositor Partidos del bloque opositor.
#' @return Tibble con `seccion` y `margen_victoria_neto`.
#' @export
calcular_margen_victoria_neto <- function(electoral_bd,
                                          elecciones,
                                          bloque_oficialista = c("morena", "pvem", "pt"),
                                          bloque_opositor = c("pan", "pri", "prd", "mc")) {
  margenes <- vapply(elecciones, function(eleccion) {
    cols <- grep(
      paste0("^ele_.*_", eleccion, "$"),
      names(electoral_bd),
      value = TRUE
    )
    col_total <- paste0("ele_total_", eleccion)
    if (length(cols) == 0 || !col_total %in% cols) {
      stop(
        "No hay columnas electorales para la elección `", eleccion,
        "` (se esperaba p. ej. `", col_total, "`)."
      )
    }
    es_bloque <- function(col, bloque) {
      partes <- partes_columna_electoral(col, eleccion)
      length(partes) > 0 && all(partes %in% bloque)
    }
    cols_of <- cols[vapply(cols, es_bloque, logical(1), bloque = bloque_oficialista)]
    cols_op <- cols[vapply(cols, es_bloque, logical(1), bloque = bloque_opositor)]

    votos_of <- rowSums(electoral_bd[, cols_of, drop = FALSE], na.rm = TRUE)
    votos_op <- rowSums(electoral_bd[, cols_op, drop = FALSE], na.rm = TRUE)
    total <- electoral_bd[[col_total]]
    ifelse(total > 0, (votos_of - votos_op) / total, NA_real_)
  }, numeric(nrow(electoral_bd)))

  margenes <- matrix(margenes, nrow = nrow(electoral_bd))
  tibble::tibble(
    seccion = electoral_bd$seccion,
    margen_victoria_neto = rowMeans(margenes, na.rm = TRUE)
  )
}

#' Participación electoral promedio por sección
#' @keywords internal
#' @noRd
calcular_participacion_seccional <- function(electoral_bd, elecciones) {
  participaciones <- vapply(elecciones, function(eleccion) {
    col_total <- paste0("ele_total_", eleccion)
    col_nominal <- paste0("ele_nominal_", eleccion)
    if (!all(c(col_total, col_nominal) %in% names(electoral_bd))) {
      return(rep(NA_real_, nrow(electoral_bd)))
    }
    total <- electoral_bd[[col_total]]
    nominal <- electoral_bd[[col_nominal]]
    ifelse(nominal > 0, total / nominal, NA_real_)
  }, numeric(nrow(electoral_bd)))

  participaciones <- matrix(participaciones, nrow = nrow(electoral_bd))
  tibble::tibble(
    seccion = electoral_bd$seccion,
    participacion_prom = rowMeans(participaciones, na.rm = TRUE)
  )
}

#' Covariables contextuales por sección electoral para DR-MNAR
#'
#' Construye la tabla de covariables seccionales de la Sección 4 del Anexo
#' Técnico a partir del censo INEGI 2020 por sección y (opcionalmente) del
#' histórico electoral y del índice de marginación de CONAPO:
#'
#' * `escolaridad_prom`: grado promedio de escolaridad (`graproes`).
#' * `pct_conectividad`: proporción de viviendas con internet
#'   (`vph_inter / vivpar_hab`), proxy de ingreso e infraestructura.
#' * `tipo_seccion`: código de tipo de sección del marco (urbano/rural/
#'   mixto según la codificación INE del año del marco).
#' * `participacion_prom` y `margen_victoria_neto`: si se entrega
#'   `electoral_bd` (ver [calcular_margen_victoria_neto()]).
#' * `indice_marginacion`: si se entrega la tabla externa de CONAPO.
#'
#' @param censo_seccion Censo por sección (llave `seccion` = "EE_SSSS").
#' @param electoral_bd Histórico electoral por sección (opcional).
#' @param elecciones Elecciones a promediar (requerido con `electoral_bd`).
#' @param marginacion Tibble opcional `seccion` + `indice_marginacion`
#'   (CONAPO; no forma parte del censo).
#' @return Tibble con una fila por sección.
#' @examples
#' \dontrun{
#' censo <- readRDS("inegi/seccion_2020.rda")
#' electoral <- readRDS("mex.rda")$info$bd
#' construir_covariables_seccion(
#'   censo_seccion = dplyr::filter(censo, entidad == "15"),
#'   electoral_bd = electoral,
#'   elecciones = c("pr_24", "gb_23", "pr_18")
#' )
#' }
#' @export
construir_covariables_seccion <- function(censo_seccion,
                                          electoral_bd = NULL,
                                          elecciones = NULL,
                                          marginacion = NULL) {
  requeridas <- c("seccion", "graproes", "vph_inter", "vivpar_hab", "tipo")
  faltan <- setdiff(requeridas, names(censo_seccion))
  if (length(faltan) > 0) {
    stop(
      "Al censo seccional le faltan columnas: ",
      paste(faltan, collapse = ", ")
    )
  }

  cov <- tibble::tibble(
    seccion = censo_seccion$seccion,
    escolaridad_prom = censo_seccion$graproes,
    pct_conectividad = ifelse(
      censo_seccion$vivpar_hab > 0,
      censo_seccion$vph_inter / censo_seccion$vivpar_hab,
      NA_real_
    ),
    tipo_seccion = factor(censo_seccion$tipo)
  )

  if (!is.null(electoral_bd)) {
    if (is.null(elecciones)) {
      stop("Especifica `elecciones` para usar el histórico electoral.")
    }
    cov <- cov |>
      dplyr::left_join(
        calcular_participacion_seccional(electoral_bd, elecciones),
        by = "seccion"
      ) |>
      dplyr::left_join(
        calcular_margen_victoria_neto(electoral_bd, elecciones),
        by = "seccion"
      )
  }

  if (!is.null(marginacion)) {
    cov <- cov |>
      dplyr::left_join(
        marginacion[, c("seccion", "indice_marginacion")],
        by = "seccion"
      )
  }

  cov
}

#' Normaliza llaves de sección al formato "EE_SSSS"
#' @keywords internal
#' @noRd
normalizar_seccion <- function(x, entidad = NULL) {
  if (is.numeric(x)) {
    if (is.null(entidad)) {
      stop(
        "La llave de sección es numérica: especifica `entidad` (p. ej. ",
        '"08") para normalizarla al formato "EE_SSSS" de las covariables.'
      )
    }
    sprintf("%s_%04d", entidad, as.integer(x))
  } else {
    as.character(x)
  }
}

#' Une las covariables seccionales a la base individual
#'
#' Pega las covariables contextuales de [construir_covariables_seccion()]
#' a cada registro individual por su sección electoral, normalizando la
#' llave (el snapshot suele traer `SECCION` numérica sin entidad) y
#' estandarizando las covariables continuas (media 0, desviación 1) para
#' estabilidad numérica de los modelos logísticos.
#'
#' @param respuestas Base individual (snapshot) con la columna de sección.
#' @param covariables_seccion Tibble de [construir_covariables_seccion()].
#' @param llave Nombre de la columna de sección en `respuestas`.
#' @param entidad Clave de entidad ("EE") para normalizar llaves numéricas.
#' @param estandarizar Estandarizar las covariables continuas (default TRUE).
#' @return `respuestas` con las covariables seccionales pegadas.
#' @export
unir_covariables_individuo <- function(respuestas, covariables_seccion,
                                       llave = "SECCION", entidad = NULL,
                                       estandarizar = TRUE) {
  if (!llave %in% names(respuestas)) {
    stop("No existe la columna `", llave, "` en la base individual.")
  }
  llave_norm <- normalizar_seccion(respuestas[[llave]], entidad = entidad)

  cov <- covariables_seccion
  unido <- respuestas |>
    dplyr::mutate(.seccion_norm = llave_norm) |>
    dplyr::left_join(cov, by = c(".seccion_norm" = "seccion")) |>
    dplyr::select(-".seccion_norm")

  cols_cov <- setdiff(names(cov), "seccion")
  sin_match <- !llave_norm %in% cov$seccion
  if (any(sin_match)) {
    message(
      sum(sin_match), " registros con sección sin covariables (",
      length(unique(llave_norm[sin_match])), " secciones); quedan en NA."
    )
  }

  if (estandarizar) {
    continuas <- cols_cov[vapply(unido[cols_cov], is.numeric, logical(1))]
    for (col in continuas) {
      v <- unido[[col]]
      desv <- stats::sd(v, na.rm = TRUE)
      if (isTRUE(desv > 0)) {
        unido[[col]] <- (v - mean(v, na.rm = TRUE)) / desv
      }
    }
  }

  unido
}
