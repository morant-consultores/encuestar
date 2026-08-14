# Cálculo de las validaciones del diagnóstico DR-MNAR: control de
# multiplicidad, contraste con los desertores del filtro temático y balance
# de covariables por brazo del instrumento.

#' Control de multiplicidad de las detecciones de no ignorabilidad
#'
#' Cada pregunta x categoría x subconjunto del diagnóstico es una prueba de
#' hipótesis, así que con decenas de pruebas a `alfa = 0.05` se esperan varias
#' señales por puro azar. Esta función deriva el p-valor de dos colas del
#' `z_stat` del diagnóstico y lo ajusta por multiplicidad.
#'
#' El método se expone en vez de fijarse porque controlan cosas distintas:
#' `"BH"` controla la tasa de falso descubrimiento (FDR) y `"holm"` la tasa de
#' error por familia (FWER), que es más estricta. La diferencia entre uno y otro
#' es información que el lector técnico necesita, no un detalle interno.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()].
#' @param metodo Método de [stats::p.adjust()] (default `"BH"`).
#' @param alfa Nivel al que se declara sobreviviente (default 0.05).
#' @return El `diagnostico` con `p_valor`, `p_ajustado` y `sobrevive`, y el
#'   atributo `metodo`.
#' @export
ajustar_multiplicidad_norespuesta <- function(diagnostico, metodo = "BH",
                                              alfa = 0.05) {
  d <- diagnostico
  d$p_valor <- 2 * stats::pnorm(-abs(d$z_stat))
  finitos <- is.finite(d$p_valor)
  d$p_ajustado <- NA_real_
  if (any(finitos)) {
    d$p_ajustado[finitos] <- stats::p.adjust(d$p_valor[finitos], method = metodo)
  }
  d$sobrevive <- !is.na(d$p_ajustado) & d$p_ajustado < alfa
  attr(d, "metodo") <- metodo
  d
}
