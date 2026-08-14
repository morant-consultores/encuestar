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
