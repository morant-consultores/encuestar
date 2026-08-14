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

#' Detecciones antes y después del control de multiplicidad
#'
#' Barras del número de detecciones crudas contra las que sobreviven al ajuste
#' de [ajustar_multiplicidad_norespuesta()], con la nota de cuántas señales se
#' esperan por azar. Es la lámina de cautela del deck: sin ella, un conteo crudo
#' de detecciones invita a leerlas todas como hallazgos firmes.
#'
#' @param tabla Salida de [ajustar_multiplicidad_norespuesta()].
#' @param alfa Nivel de significancia usado (default 0.05).
#' @return Objeto [ggplot2::ggplot], o `NULL` si no hay pruebas estimables.
#' @export
graficar_multiplicidad_norespuesta <- function(tabla, alfa = 0.05) {
  n_pruebas <- sum(is.finite(tabla$p_valor))
  if (n_pruebas == 0) return(NULL)

  metodo <- attr(tabla, "metodo") %||% "BH"
  n_crudo <- sum(tabla$p_valor < alfa, na.rm = TRUE)
  n_ajustado <- sum(tabla$sobrevive, na.rm = TRUE)
  esperados <- round(n_pruebas * alfa, 1)

  niveles <- c("Sin ajuste", paste0("Ajustado (", metodo, ")"))
  bd <- tibble::tibble(
    criterio = factor(niveles, levels = niveles),
    n = c(n_crudo, n_ajustado)
  )

  ggplot(bd, aes(x = .data$criterio, y = .data$n, fill = .data$criterio)) +
    geom_col(width = 0.5) +
    geom_hline(yintercept = esperados, linetype = "dashed",
               color = COLOR_NEUTRO) +
    geom_text(aes(label = .data$n), vjust = -0.4, size = 6, fontface = "bold") +
    scale_fill_manual(
      values = stats::setNames(c(COLOR_NEUTRO, COLOR_MORANT), niveles),
      guide = "none"
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    labs(
      x = NULL, y = "detecciones",
      subtitle = stringr::str_wrap(sprintf(
        paste0("Con %d pruebas a alfa = %s se esperan ~%s señales por azar ",
               "(línea punteada). Sobreviven %d al ajuste por multiplicidad ",
               "(%s). Solo esas deben leerse como sesgo estructural; el resto ",
               "se confirma con más muestra en olas posteriores."),
        n_pruebas, alfa, esperados, n_ajustado, metodo), 95)
    ) +
    tema_morant()
}

#' Validación del diagnóstico con los desertores del filtro
#'
#' Dumbbell de la proporción entre quienes eligieron el módulo político y
#' quienes desertaron. Cuando se pasa el diagnóstico, marca por pregunta si el
#' signo del contraste COINCIDE con el de `gamma_y`: la coincidencia es
#' validación cruzada del modelo MNAR y la discrepancia se muestra tal cual, no
#' se oculta.
#'
#' @param tabla Salida de [comparar_desertores_norespuesta()].
#' @param diagnostico Tibble de [diagnosticar_norespuesta()] (opcional).
#' @return Objeto [ggplot2::ggplot], o `NULL` si la tabla viene vacía.
#' @export
graficar_desertores_norespuesta <- function(tabla, diagnostico = NULL) {
  if (nrow(tabla) == 0) return(NULL)

  bd <- tabla
  bd$valida <- NA
  if (!is.null(diagnostico) && nrow(diagnostico) > 0) {
    g <- diagnostico$gamma_y[match(bd$pregunta, diagnostico$pregunta)]
    bd$valida <- ifelse(is.finite(g), sign(bd$diferencia) == sign(g), NA)
  }
  bd$marca <- dplyr::case_when(
    is.na(bd$valida) ~ "Sin gamma comparable",
    bd$valida ~ "Coincide con gamma",
    .default = "Discrepa de gamma"
  )
  bd$etiqueta <- stringr::str_wrap(
    paste0(bd$pregunta, ": ", bd$categoria), 28)

  largo <- bd |>
    tidyr::pivot_longer(
      cols = c("p_voluntario", "p_desertor"),
      names_to = "grupo", values_to = "p"
    ) |>
    dplyr::mutate(
      grupo = ifelse(.data$grupo == "p_voluntario",
                     "Eligió el módulo político", "Desertó al menú temático")
    )

  ggplot(largo, aes(y = stats::reorder(.data$etiqueta, .data$p))) +
    geom_segment(
      data = bd,
      aes(x = .data$p_desertor, xend = .data$p_voluntario,
          y = .data$etiqueta, yend = .data$etiqueta,
          color = .data$marca),
      linewidth = 1.1
    ) +
    geom_point(aes(x = .data$p, shape = .data$grupo), size = 4,
               color = COLOR_NEUTRO) +
    scale_color_manual(
      values = c("Coincide con gamma" = COLOR_MORANT,
                 "Discrepa de gamma" = "#8A5E06",
                 "Sin gamma comparable" = COLOR_NEUTRO),
      name = NULL
    ) +
    scale_shape_manual(
      values = c("Eligió el módulo político" = 16,
                 "Desertó al menú temático" = 1),
      name = NULL
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    tema_morant() +
    theme(legend.position = "bottom", legend.box = "vertical")
}

# El paquete declara R (>= 2.10), así que no se puede asumir el `%||%` de base
# (llegó en R 4.4). Se define aquí porque el módulo lo usa en cada default que
# sale del bundle o de un atributo.
`%||%` <- function(a, b) if (is.null(a)) b else a
