# Gráficas de validación e impacto del diagnóstico DR-MNAR.

#' Impacto práctico de corregir por no respuesta no ignorable
#'
#' Dumbbell del corrimiento de la estimación al pasar del raking tradicional
#' (`Weights-MAR`) al doblemente robusto MNAR, para las preguntas cuya decisión
#' es DR-MNAR. Responde la pregunta que el caterpillar de gamma no responde:
#' cuánto se mueve el número que va al reporte.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()] con `est_rake` y
#'   `est_drmnar`.
#' @return Objeto [ggplot2::ggplot], o `NULL` si no hay filas DR-MNAR con
#'   estimaciones finitas.
#' @export
graficar_impacto_drmnar <- function(diagnostico) {
  bd <- diagnostico |>
    dplyr::filter(
      .data$decision == "DR-MNAR",
      is.finite(.data$est_rake), is.finite(.data$est_drmnar)
    )
  if (nrow(bd) == 0) return(NULL)

  bd <- bd |>
    dplyr::mutate(
      etiqueta = stringr::str_wrap(
        ifelse(is.na(.data$categoria), .data$pregunta,
               paste0(.data$pregunta, ": ", .data$categoria)), 28),
      pp = (.data$est_drmnar - .data$est_rake) * 100
    )

  largo <- bd |>
    tidyr::pivot_longer(
      cols = c("est_rake", "est_drmnar"),
      names_to = "estimador", values_to = "est"
    ) |>
    dplyr::mutate(
      estimador = ifelse(.data$estimador == "est_rake",
                         "Raking (MAR)", "DR-MNAR")
    )

  ggplot(largo, aes(y = stats::reorder(.data$etiqueta, .data$est))) +
    geom_segment(
      data = bd,
      aes(x = .data$est_rake, xend = .data$est_drmnar,
          y = .data$etiqueta, yend = .data$etiqueta),
      linewidth = 1.1, color = COLOR_NEUTRO
    ) +
    geom_point(aes(x = .data$est, color = .data$estimador), size = 4.2) +
    geom_text(
      data = bd,
      aes(x = pmax(.data$est_rake, .data$est_drmnar), y = .data$etiqueta,
          label = sprintf("%+.1f pp", .data$pp)),
      hjust = -0.25, size = 4.3, fontface = "bold", color = COLOR_MORANT
    ) +
    scale_color_manual(
      values = c("Raking (MAR)" = COLOR_NEUTRO, "DR-MNAR" = COLOR_MORANT),
      name = NULL
    ) +
    scale_x_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0.06, 0.18))
    ) +
    labs(x = NULL, y = NULL) +
    tema_morant() +
    theme(legend.position = "bottom")
}

#' Costo en precisión de los estimadores MNAR
#'
#' Barras del error estándar (sándwich, agrupado por UPM y estrato) de cada uno
#' de los siete estimadores de [estimar_drmnar()]. Corregir el sesgo de
#' selección no sale gratis: la estimación en dos etapas del modelo MNAR agrega
#' variabilidad, y esta lámina la deja explícita para que el intervalo del
#' reporte no se lea con más confianza de la que tiene.
#'
#' @param estimaciones Tibble de [estimar_drmnar()] (una pregunta).
#' @return Objeto [ggplot2::ggplot], o `NULL` si ningún error estándar es
#'   finito.
#' @export
graficar_precision_drmnar <- function(estimaciones) {
  orden <- c("Observado", "Weights-MAR", "Imput-MAR", "DR-MAR",
             "Weights-MNAR", "Imput-MNAR", "DR-MNAR")
  bd <- estimaciones |> dplyr::filter(is.finite(.data$ee))
  if (nrow(bd) == 0) return(NULL)

  bd <- bd |>
    dplyr::mutate(
      modelo = factor(.data$modelo, levels = rev(orden)),
      tipo = ifelse(grepl("MNAR", .data$modelo), "MNAR", "MAR/Observado")
    )

  ggplot(bd, aes(x = .data$modelo, y = .data$ee, fill = .data$tipo)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::percent(.data$ee, accuracy = 0.01)),
      hjust = -0.15, size = 4.3
    ) +
    scale_fill_manual(
      values = c("MAR/Observado" = COLOR_NEUTRO, "MNAR" = COLOR_MORANT),
      name = NULL
    ) +
    scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    coord_flip() +
    labs(x = NULL, y = "Error estándar sándwich (agrupado por UPM y estrato)") +
    tema_morant() +
    theme(legend.position = "bottom")
}
