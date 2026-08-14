# Traducción de gamma a lenguaje de negocio para las láminas del deck.

#' Lectura en lenguaje de negocio del parámetro de no ignorabilidad
#'
#' Traduce cada fila del diagnóstico a una frase accionable según el signo de
#' `gamma_y` y su significancia. Tres estados excluyentes:
#'
#' * `sobre_representacion` (gamma > 0, IC excluye 0): quienes están en la
#'   categoría responden más, así que el raking SOBREESTIMA el indicador.
#' * `sub_representacion` (gamma < 0, IC excluye 0): quienes están en la
#'   categoría responden menos (ocultamiento), así que el raking SUBESTIMA.
#' * `ignorable` (IC incluye 0, gamma no estimable o sin convergencia): la no
#'   respuesta es ignorable y el raking es adecuado y más eficiente.
#'
#' El texto se arma desde los datos del corte, nunca desde una tabla fija, para
#' que no se desincronice cuando cambian los números.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()].
#' @return Tibble con `pregunta`, `categoria`, `subconjunto`, `estado` y `texto`.
#' @export
lectura_norespuesta <- function(diagnostico) {
  d <- diagnostico
  vacio <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    subconjunto = character(0), estado = character(0), texto = character(0)
  )
  if (nrow(d) == 0) return(vacio)

  conv <- if ("convergencia" %in% names(d)) {
    !is.na(d$convergencia) & d$convergencia
  } else {
    TRUE
  }
  estimable <- is.finite(d$gamma_y) & is.finite(d$inf) & is.finite(d$sup) & conv
  significativo <- estimable & (d$inf > 0 | d$sup < 0)

  estado <- ifelse(
    !significativo, "ignorable",
    ifelse(d$gamma_y > 0, "sobre_representacion", "sub_representacion")
  )

  etiqueta <- ifelse(
    is.na(d$categoria) | !nzchar(d$categoria),
    d$pregunta, paste0(d$pregunta, " = \"", d$categoria, "\"")
  )

  # el corrimiento solo se menciona si el diagnóstico lo trae calculado
  pp <- if (all(c("est_rake", "est_drmnar") %in% names(d))) {
    (d$est_drmnar - d$est_rake) * 100
  } else {
    rep(NA_real_, nrow(d))
  }
  frase_pp <- ifelse(
    is.finite(pp),
    sprintf(" El ajuste mueve la estimación %.1f puntos porcentuales.", pp),
    ""
  )

  texto <- dplyr::case_when(
    estado == "sobre_representacion" ~ sprintf(
      paste0("%s: quienes responden esta categoría participan MÁS en la ",
             "encuesta (gamma = %+.2f), así que el raking SOBREESTIMA el ",
             "indicador.%s"),
      etiqueta, d$gamma_y, frase_pp),
    estado == "sub_representacion" ~ sprintf(
      paste0("%s: quienes responden esta categoría participan MENOS ",
             "(ocultamiento, gamma = %+.2f), así que el raking SUBESTIMA el ",
             "indicador.%s"),
      etiqueta, d$gamma_y, frase_pp),
    .default = sprintf(
      paste0("%s: no respuesta ignorable, el intervalo de gamma incluye el 0. ",
             "El raking es adecuado y más eficiente (menor varianza)."),
      etiqueta)
  )

  tibble::tibble(
    pregunta = d$pregunta, categoria = d$categoria,
    subconjunto = d$subconjunto, estado = estado, texto = texto
  )
}
