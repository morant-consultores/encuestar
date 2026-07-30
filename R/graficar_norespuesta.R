# Gráficas diagnóstico de no respuesta no ignorable (DR-MNAR) ------------
#
# Visualizaciones con la identidad Morant (tema_morant) para el análisis
# de no respuesta: caterpillar del parámetro crítico gamma_y por pregunta
# y subconjunto, comparación de los 7 estimadores (Figura 1 de Bailey),
# flujo de tamaños de muestra por brazo (Figura 2) y distribución de los
# pesos de propensión inversa.

COLOR_MORANT <- "#A6032F"
COLOR_NEUTRO <- "#4C5B61"

#' Caterpillar del parámetro de no ignorabilidad por pregunta y subconjunto
#'
#' Gráfica principal del diagnóstico: intervalo de confianza de `gamma_y`
#' por pregunta (facetes por subconjunto si hay más de uno) con la línea
#' de referencia en 0. Las preguntas cuyo intervalo excluye el 0 (no
#' respuesta no ignorable => se activa DR-MNAR) se destacan en rojo
#' Morant.
#'
#' @param bd Tibble de [diagnosticar_norespuesta()].
#' @param salto Ancho de envoltura del nombre de la pregunta.
#' @return Objeto [ggplot2::ggplot].
#' @export
graficar_diagnostico_norespuesta <- function(bd, salto = 25) {
  bd <- bd |>
    dplyr::filter(!is.na(gamma_y)) |>
    dplyr::mutate(
      etiqueta = stringr::str_wrap(
        ifelse(
          is.na(categoria),
          pregunta,
          paste0(pregunta, ": ", categoria)
        ),
        salto
      )
    )

  g <- bd |>
    ggplot(aes(
      x = stats::reorder(etiqueta, gamma_y),
      y = gamma_y,
      color = no_ignorable
    )) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_NEUTRO) +
    geom_pointrange(aes(ymin = inf, ymax = sup), linewidth = 1, size = 0.9) +
    geom_text(
      aes(label = round(gamma_y, 2)),
      nudge_x = 0.32, size = 4.5, show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("FALSE" = COLOR_NEUTRO, "TRUE" = COLOR_MORANT),
      labels = c("FALSE" = "Ignorable (Raking)", "TRUE" = "No ignorable (DR-MNAR)"),
      name = NULL
    ) +
    coord_flip() +
    labs(
      x = NULL,
      y = expression(gamma[Y] ~ "(parámetro de no ignorabilidad, IC 95%)")
    ) +
    tema_morant() +
    theme(legend.position = "bottom")

  if (length(unique(bd$subconjunto)) > 1) {
    g <- g + facet_wrap(~subconjunto)
  }
  g
}

#' Comparación de estimadores MAR/MNAR (Figura 1 de Bailey)
#'
#' Pointrange de la estimación puntual e IC de los 7 estimadores de
#' [estimar_drmnar()] para una pregunta, en el orden metodológico
#' (Observado -> ponderados/imputación/DR MAR -> MNAR). Permite leer
#' cuánto se modifica la estimación al corregir por no respuesta no
#' ignorable respecto al flujo normal (svymean + rake = Weights-MAR).
#'
#' @param bd Tibble de [estimar_drmnar()] (una pregunta).
#' @return Objeto [ggplot2::ggplot].
#' @export
graficar_comparacion_estimadores <- function(bd) {
  orden <- c(
    "Observado", "Weights-MAR", "Imput-MAR", "DR-MAR",
    "Weights-MNAR", "Imput-MNAR", "DR-MNAR"
  )
  bd <- bd |>
    dplyr::mutate(
      modelo = factor(modelo, levels = rev(orden)),
      tipo = ifelse(grepl("MNAR", modelo), "MNAR", "MAR/Observado")
    )

  titulo <- unique(bd$pregunta)
  subtitulo <- unique(bd$subconjunto)

  bd |>
    ggplot(aes(x = modelo, y = est, color = tipo)) +
    geom_pointrange(aes(ymin = inf, ymax = sup), linewidth = 1, size = 0.9) +
    geom_text(
      aes(label = scales::percent(est, accuracy = 0.1)),
      nudge_x = 0.35, size = 4.5, show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("MAR/Observado" = COLOR_NEUTRO, "MNAR" = COLOR_MORANT),
      name = NULL
    ) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(x = NULL, y = NULL, title = titulo, subtitle = subtitulo) +
    tema_morant() +
    theme(legend.position = "bottom")
}

#' Flujo de tamaños de muestra por brazo del instrumento (Figura 2)
#'
#' Barras del flujo experimental: contactos por brazo, respuesta al módulo
#' político y desglose de la deserción del filtro temático.
#'
#' @param flujo Tibble de [resumen_flujo_norespuesta()].
#' @return Objeto [ggplot2::ggplot].
#' @export
graficar_flujo_norespuesta <- function(flujo) {
  bd <- flujo |>
    dplyr::mutate(
      etiqueta = ifelse(
        is.na(detalle),
        paste(brazo, etapa, sep = " — "),
        paste0(brazo, " — desertó: ", detalle)
      ),
      grupo = ifelse(etapa == "Desertó del filtro", "Deserción", etapa)
    )

  bd |>
    ggplot(aes(
      x = stats::reorder(stringr::str_wrap(etiqueta, 30), n),
      y = n,
      fill = grupo
    )) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.15, size = 4.5) +
    scale_fill_manual(
      values = c(
        "Contactos" = COLOR_NEUTRO,
        "Respondió módulo político" = "#2C423F",
        "Deserción" = COLOR_MORANT
      ),
      name = NULL
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    coord_flip() +
    labs(x = NULL, y = "Registros") +
    tema_morant() +
    theme(legend.position = "bottom")
}

#' Distribución de los pesos de propensión inversa MNAR
#'
#' Densidad de los pesos `1/pi` del modelo de propensión MNAR. Pesos
#' extremos señalan inestabilidad del estimador IPW (revisar covariables o
#' considerar el DR, que amortigua el problema al ponderar el residuo).
#'
#' @param pesos Vector numérico de pesos `1/pi` (respondientes).
#' @param umbral Umbral de alerta para pesos extremos (default 10).
#' @return Objeto [ggplot2::ggplot].
#' @export
graficar_pesos_drmnar <- function(pesos, umbral = 10) {
  bd <- tibble::tibble(peso = pesos[is.finite(pesos)])
  extremos <- sum(bd$peso > umbral)

  bd |>
    ggplot(aes(x = peso)) +
    geom_density(fill = COLOR_MORANT, alpha = 0.35, color = COLOR_MORANT) +
    geom_rug(alpha = 0.25, color = COLOR_NEUTRO) +
    geom_vline(xintercept = umbral, linetype = "dashed", color = COLOR_NEUTRO) +
    labs(
      x = "Peso de propensión inversa (1/π)",
      y = NULL,
      subtitle = paste0(
        extremos, " pesos por arriba del umbral (", umbral, ")"
      )
    ) +
    tema_morant()
}

#' Tabla de covariables de la fusión DR-MNAR (lámina del deck diagnóstico)
#'
#' Dibuja como lámina el catálogo de covariables usadas en el ajuste
#' (individuales del cuestionario y/o contextuales censales por AGEB) y
#' anota si el vector ENRIQUECIDO (fusión multinivel) está activo o en
#' fallback según el umbral de n. Es la lámina de portada metodológica del
#' deck de diagnóstico de no respuesta; se movió aquí (antes vivía inline en
#' el script `press_norespuesta` de cada proyecto) para que el deck use puras
#' funciones del paquete.
#'
#' @param doc `data.frame` con una fila por covariable y columnas
#'   `covariable`, `tipo`, `fuente`, `mide` (p. ej. el `covariables_doc` del
#'   bundle de [generar_diseno_drmnar()]).
#' @param n_ef Número de efectivas del corte.
#' @param umbral n a partir del cual se activa el vector enriquecido.
#' @param ricas Vector de covariables del esquema enriquecido (para la nota
#'   de fallback).
#' @return Objeto [ggplot2::ggplot].
#' @export
graficar_tabla_covariables <- function(doc, n_ef, umbral, ricas) {
  n <- nrow(doc); doc$y <- if (n > 0) n:1 else numeric(0)
  encabez <- data.frame(x = c(0.00, 0.19, 0.33, 0.55),
                        lab = c("Covariable", "Tipo", "Fuente", "Qué ajusta / mide"))
  activo  <- n_ef >= umbral
  cap <- if (activo) {
    sprintf("Vector ENRIQUECIDO activo (n = %s >= %s efectivas): fusion individual + contextual censal por AGEB.",
            format(n_ef, big.mark = ","), format(umbral, big.mark = ","))
  } else {
    sprintf("Vector en FALLBACK (n = %s < %s). A partir de %s efectivas se activa la fusion: %s.",
            format(n_ef, big.mark = ","), format(umbral, big.mark = ","),
            format(umbral, big.mark = ","), paste(ricas, collapse = " + "))
  }
  ggplot() +
    annotate("text", x = encabez$x, y = n + 1, label = encabez$lab,
             hjust = 0, fontface = "bold", size = 5, color = COLOR_MORANT) +
    geom_hline(yintercept = n + 0.55, color = COLOR_NEUTRO, linewidth = 0.5) +
    geom_text(data = doc, aes(x = 0.00, y = .data$y, label = .data$covariable),
              hjust = 0, size = 4.7, fontface = "bold", color = COLOR_NEUTRO) +
    geom_text(data = doc, aes(x = 0.19, y = .data$y, label = .data$tipo),
              hjust = 0, size = 4.2, color = "#555555") +
    geom_text(data = doc, aes(x = 0.33, y = .data$y, label = .data$fuente),
              hjust = 0, size = 4.2, color = "#555555") +
    geom_text(data = doc, aes(x = 0.55, y = .data$y,
              label = stringr::str_wrap(.data$mide, 38)), hjust = 0, size = 4.2, color = "#333333") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0.4, n + 1.6)) +
    labs(caption = stringr::str_wrap(cap, 120)) +
    theme_void() +
    theme(plot.caption = element_text(hjust = 0, size = 12, color = "#666666",
                                      margin = ggplot2::margin(t = 14)))
}

# Clase R6 -----------------------------------------------------------------

#' Análisis de no respuesta no ignorable (DR-MNAR)
#'
#' Clase de análisis del hub `Resultados` que orquesta el diagnóstico de
#' no respuesta no ignorable, la estimación doblemente robusta y sus
#' gráficas sobre el diseño muestral de la encuesta. Requiere que las
#' variables del diseño incluyan el instrumento aleatorizado
#' (ver [preparar_variables_drmnar()]).
#'
#' @field encuesta Objeto `Encuesta` (opcional).
#' @field diseno Diseño muestral `survey::svydesign`.
#' @field diccionario Diccionario del cuestionario (opcional).
#' @field tema Tema gráfico.
#' @field covariables Covariables default de los modelos DR-MNAR.
#' @field instrumento Columna del brazo aleatorizado.
#' @field ultimo_diagnostico Cache del último diagnóstico calculado.
#' @export
NoRespuesta <- R6::R6Class(
  classname = "NoRespuesta",
  public = list(
    encuesta = NULL,
    diseno = NULL,
    diccionario = NULL,
    tema = NULL,
    covariables = NULL,
    instrumento = NULL,
    ultimo_diagnostico = NULL,
    #' @description Inicializa el análisis de no respuesta.
    #' @param encuesta Objeto `Encuesta` (opcional; provee el diseño).
    #' @param diseno Diseño muestral; si es `NULL` se toma de la encuesta.
    #' @param diccionario Diccionario (opcional).
    #' @param tema Tema gráfico (default [tema_morant()]).
    #' @param covariables Covariables default de los modelos.
    #' @param instrumento Columna del brazo aleatorizado.
    initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL,
                          tema = tema_morant(), covariables = NULL,
                          instrumento = "drmnar_z") {
      self$encuesta <- encuesta
      self$diseno <- if (is.null(diseno) && !is.null(encuesta)) {
        encuesta$muestra$diseno
      } else {
        diseno
      }
      self$diccionario <- diccionario
      self$tema <- tema
      self$covariables <- covariables
      self$instrumento <- instrumento
    },
    #' @description Diagnóstico de gamma por pregunta y subconjunto
    #'  ([diagnosticar_norespuesta()]); se cachea para las gráficas.
    #' @param preguntas Lista nombrada `list(pregunta = categoria(s))`.
    #' @param subconjuntos Lista nombrada de vectores lógicos.
    #' @param covariables Covariables (default las de la clase).
    #' @param ... Argumentos adicionales.
    diagnostico = function(preguntas, subconjuntos = NULL,
                           covariables = self$covariables, ...) {
      self$ultimo_diagnostico <- diagnosticar_norespuesta(
        diseno = self$diseno, preguntas = preguntas,
        covariables = covariables, instrumento = self$instrumento,
        subconjuntos = subconjuntos, ...
      )
      self$ultimo_diagnostico
    },
    #' @description Caterpillar de gamma ([graficar_diagnostico_norespuesta()]).
    #' @param diagnostico Tibble de diagnóstico (default: el último).
    #' @param ... Argumentos para la gráfica.
    grafica_diagnostico = function(diagnostico = NULL, ...) {
      bd <- if (is.null(diagnostico)) self$ultimo_diagnostico else diagnostico
      if (is.null(bd)) {
        stop("Corre primero $diagnostico(preguntas = ...).")
      }
      graficar_diagnostico_norespuesta(bd, ...)
    },
    #' @description Comparación de 7 estimadores ([estimar_drmnar()]).
    #' @param pregunta Pregunta a estimar.
    #' @param categoria Categoría que define y = 1.
    #' @param subconjunto Vector lógico opcional.
    #' @param covariables Covariables (default las de la clase).
    #' @param ... Argumentos adicionales.
    estimacion = function(pregunta, categoria = NULL, subconjunto = NULL,
                          covariables = self$covariables, ...) {
      estimar_drmnar(
        diseno = self$diseno, pregunta = pregunta,
        covariables = covariables, instrumento = self$instrumento,
        categoria = categoria, subconjunto = subconjunto, ...
      )
    },
    #' @description Gráfica de la comparación de estimadores (Figura 1).
    #' @param pregunta Pregunta a estimar.
    #' @param categoria Categoría que define y = 1.
    #' @param ... Argumentos para [NoRespuesta$estimacion()].
    grafica_comparacion = function(pregunta, categoria = NULL, ...) {
      graficar_comparacion_estimadores(
        self$estimacion(pregunta = pregunta, categoria = categoria, ...)
      ) +
        self$tema
    },
    #' @description Flujo de tamaños de muestra por brazo (Figura 2).
    flujo = function() {
      resumen_flujo_norespuesta(self$diseno$variables)
    },
    #' @description Gráfica del flujo por brazo.
    grafica_flujo = function() {
      graficar_flujo_norespuesta(self$flujo())
    },
    #' @description Descriptivos del experimento (Table A16).
    #' @param subconjuntos Lista nombrada de vectores lógicos.
    descriptivos = function(subconjuntos = list("Todos" = NULL)) {
      tabla_descriptivos_drmnar(self$diseno$variables, subconjuntos)
    }
  )
)
