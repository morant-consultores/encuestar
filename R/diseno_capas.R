# Arquitectura de pesos por capas ----------------------------------------
#
# Implementación de la decisión registrada en
# doubly robust estimator/docs/DECISION_ARQUITECTURA_PESOS.md (2026-07-07):
# el peso base refleja SOLO la selección planeada; la no respuesta se
# corrige en capas declaradas y coordinadas, no dentro del peso de diseño.
#
#   Capa 1 — SELECCIÓN PLANEADA:  w1_s = LN_s / (pi_s * n_plan_s)
#     pi_s = probabilidad de inclusión de la sección (PPS del sorteo);
#     n_plan_s = entrevistas planeadas en la sección. Bajo PPS con cuota
#     fija por sección el diseño es epsem y w1 es ~constante.
#   Capa 2 — CLASE DE NO RESPUESTA POR SECCIÓN:  f_s = min(n_plan/logradas, tope)
#     el ajuste que antes se escondía en el "fpc empírico" (asumiendo MCAR
#     sin decirlo y sin tope), ahora declarado, acotado y auditable.
#   Capa 3 — RAKE demográfico a márgenes poblacionales, con monitoreo de
#     factores (sin cuotas en campo, el rake carga la composición).
#
# La corrección por no respuesta NO IGNORABLE (gamma != 0) no vive aquí:
# es la capa 4 (DR-MNAR por estimando, ver generar_diseno_drmnar()).

#' Diseño muestral por capas: selección planeada + clases de no respuesta + rake
#'
#' Construye el `survey::svydesign` de la nueva arquitectura de pesos a
#' partir del snapshot de efectivas y el plan muestral por sección,
#' reemplazando la ruta del `fpc` empírico (que convertía la no respuesta
#' en "etapa de muestreo" bajo un supuesto MCAR refutado por el
#' diagnóstico DR-MNAR).
#'
#' @param bd Snapshot de entrevistas efectivas (una fila por entrevista).
#' @param plan Tibble del plan muestral por sección con columnas
#'   `seccion`, `pi_seccion` (probabilidad de inclusión del sorteo),
#'   `ln_seccion` (lista nominal o medida de tamaño; opcional pero
#'   recomendada) y `n_plan` (entrevistas planeadas).
#' @param seccion Nombre de la columna de sección en `bd`.
#' @param tope_clase Tope del factor de clase de no respuesta
#'   (default 3: una sección con menos de un tercio del plan no infla más).
#' @param margenes Lista nombrada de data.frames `variable, Freq` para el
#'   rake (p. ej. `list(sexo = ..., rango_edad = ...)`). `NULL` = sin rake.
#' @param estrato Columna de estrato en `bd` (opcional).
#' @param umbral_rake Rango aceptable de los factores de rake
#'   (composición); fuera de él se emite warning operativo.
#' @return `survey.design` con UPM = sección y atributo `"capas"`: tabla
#'   por sección con `w_seleccion`, `logradas`, `factor_clase` (auditoría
#'   de cada capa).
#' @export
construir_diseno_capas <- function(bd, plan, seccion = "SECCION",
                                   tope_clase = 3,
                                   margenes = NULL,
                                   estrato = NULL,
                                   umbral_rake = c(0.5, 2)) {
  requeridas <- c("seccion", "pi_seccion", "n_plan")
  faltan <- setdiff(requeridas, names(plan))
  if (length(faltan) > 0) {
    stop("Al plan le faltan columnas: ", paste(faltan, collapse = ", "))
  }
  if (!seccion %in% names(bd)) {
    stop("No existe la columna `", seccion, "` en el snapshot.")
  }

  bd$.seccion <- as.character(bd[[seccion]])
  plan <- plan |>
    dplyr::mutate(seccion = as.character(seccion))

  # ---- Capa 1: peso de selección planeada ----
  plan <- plan |>
    dplyr::mutate(
      w_seleccion = if ("ln_seccion" %in% names(plan)) {
        ln_seccion / (pi_seccion * n_plan)
      } else {
        1 / pi_seccion
      }
    )

  logradas <- bd |>
    dplyr::count(.seccion, name = "logradas")

  capas <- logradas |>
    dplyr::left_join(plan, by = c(".seccion" = "seccion")) |>
    dplyr::rename(seccion = ".seccion")

  # secciones levantadas fuera del plan (sustituciones no registradas,
  # llaves rotas): peso de selección mediano + plan default, con aviso
  fuera <- is.na(capas$w_seleccion)
  if (any(fuera)) {
    warning(
      sum(fuera), " sección(es) del snapshot fuera del plan muestral (",
      paste(utils::head(capas$seccion[fuera], 5), collapse = ", "),
      if (sum(fuera) > 5) ", ..." else "",
      "): reciben el peso de selección mediano. Documenta la causa ",
      "(sustitución en campo o llave rota).",
      call. = FALSE
    )
    capas$w_seleccion[fuera] <- stats::median(capas$w_seleccion[!fuera])
    capas$n_plan[fuera] <- stats::median(capas$n_plan[!fuera])
  }

  # ---- Capa 2: clase de no respuesta por sección (declarada y con tope) ----
  # f_s = min(n_plan / logradas_s, tope)
  #
  # SIN piso en 1. Antes había un `pmax(..., 1)` que impedía que el factor
  # bajara de 1: cuando una sección levantaba MÁS de lo planeado el exceso no
  # deflactaba y la sección terminaba representando tantas veces su población
  # como veces se hubiera excedido (medido en el 309: secciones a >3x del plan
  # decían representar 242 mil personas contra 24 mil las que iban en plan).
  # La población de una sección es FIJA: no crece porque se toquen más puertas,
  # así que sobre-ejecutar debe deflactar. Con este cambio el peso de una
  # sección sobre-ejecutada es ln/(pi * logradas) y su total es INVARIANTE al
  # número de entrevistas.
  #
  # El tope aplica SOLO a la inflación por sub-ejecución (factor > 1) y puede
  # venir POR SECCIÓN si el plan trae la columna `tope_seccion`. Eso permite
  # distinguir la sección CERRADA —agotó su presupuesto de puertas, su déficit
  # es no respuesta real y merece el factor completo (tope Inf)— de la que
  # sigue en campo, que está incompleta y sí conviene topar para que una
  # sección con 1 entrevista no cargue el peso de 20.
  capas <- capas |>
    dplyr::mutate(
      .tope = if ("tope_seccion" %in% names(capas)) tope_seccion else tope_clase,
      .tope = dplyr::coalesce(.tope, tope_clase),
      factor_clase = pmin(n_plan / logradas, .tope),
      peso_seccion = w_seleccion * factor_clase
    )

  bd$.peso <- capas$peso_seccion[match(bd$.seccion, capas$seccion)]

  # ---- diseño con UPM = sección ----
  diseno <- if (is.null(estrato)) {
    survey::svydesign(ids = ~.seccion, weights = ~.peso, data = bd)
  } else {
    bd$.estrato <- as.character(bd[[estrato]])
    survey::svydesign(ids = ~.seccion, strata = ~.estrato,
                      weights = ~.peso, data = bd, nest = TRUE)
  }

  # ---- Capa 3: rake con monitoreo de factores ----
  if (!is.null(margenes)) {
    formulas <- lapply(names(margenes), function(v) stats::as.formula(paste0("~", v)))
    diseno_rake <- survey::rake(diseno, formulas, unname(margenes))
    factor_rake <- as.numeric(stats::weights(diseno_rake)) /
      as.numeric(stats::weights(diseno))
    factor_rake <- factor_rake / stats::median(factor_rake)
    message(sprintf(
      "Factores de rake (composición): min %.2f | mediana %.2f | max %.2f.",
      min(factor_rake), stats::median(factor_rake), max(factor_rake)
    ))
    if (min(factor_rake) < umbral_rake[1] || max(factor_rake) > umbral_rake[2]) {
      warning(sprintf(
        paste0(
          "Factores de rake fuera del umbral [%.2f, %.2f] (min %.2f, ",
          "max %.2f): campo desbalanceado; revisar pases de horario."
        ),
        umbral_rake[1], umbral_rake[2], min(factor_rake), max(factor_rake)
      ), call. = FALSE)
    }
    diseno <- diseno_rake
  }

  attr(diseno, "capas") <- capas
  diseno
}
