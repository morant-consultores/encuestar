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

#' Contraste con los desertores del filtro temático (continuum of resistance)
#'
#' Dentro del brazo de tratamiento, compara la proporción de cada categoría
#' entre quienes eligieron el módulo político (`drmnar_r == 1`) y quienes
#' desertaron al menú temático (`drmnar_r == 0`) pero aun así traen respuesta.
#'
#' Es una validación externa del modelo MNAR: si el signo de
#' `p_voluntario - p_desertor` coincide con el de `gamma_y`, dos rutas
#' independientes —el modelo de propensión y una diferencia de proporciones sin
#' modelo— llegan a la misma conclusión.
#'
#' @param bd `data.frame` del snapshot con `drmnar_z`, `drmnar_r` y las
#'   preguntas.
#' @param preguntas Lista nombrada `list(pregunta = categoria)`.
#' @return Tibble con `pregunta`, `categoria`, `n_voluntario`, `n_desertor`,
#'   `p_voluntario`, `p_desertor`, `diferencia` y `p_valor`. Cero filas si no
#'   hay desertores con respuesta.
#' @export
comparar_desertores_norespuesta <- function(bd, preguntas) {
  vacio <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    n_voluntario = integer(0), n_desertor = integer(0),
    p_voluntario = numeric(0), p_desertor = numeric(0),
    diferencia = numeric(0), p_valor = numeric(0)
  )
  if (!all(c("drmnar_z", "drmnar_r") %in% names(bd))) {
    stop("Corre primero preparar_variables_drmnar() sobre el snapshot.",
         call. = FALSE)
  }
  trat <- bd[bd$drmnar_z == 1, , drop = FALSE]
  if (nrow(trat) == 0) return(vacio)

  filas <- list()
  for (preg in names(preguntas)) {
    if (!preg %in% names(trat)) next
    cat_i <- preguntas[[preg]]
    val <- trat[[preg]]
    obs <- !is.na(val)
    vol <- obs & trat$drmnar_r == 1
    des <- obs & trat$drmnar_r == 0
    if (sum(vol) == 0 || sum(des) == 0) next

    y <- val %in% cat_i
    exitos <- c(sum(y & vol), sum(y & des))
    totales <- c(sum(vol), sum(des))
    pv <- tryCatch(
      stats::prop.test(exitos, totales)$p.value,
      error = function(e) NA_real_, warning = function(w) NA_real_
    )
    filas[[length(filas) + 1]] <- tibble::tibble(
      pregunta = preg,
      categoria = paste(cat_i, collapse = " | "),
      n_voluntario = as.integer(totales[1]),
      n_desertor = as.integer(totales[2]),
      p_voluntario = exitos[1] / totales[1],
      p_desertor = exitos[2] / totales[2],
      diferencia = exitos[1] / totales[1] - exitos[2] / totales[2],
      p_valor = pv
    )
  }
  if (length(filas) == 0) return(vacio)
  dplyr::bind_rows(filas)
}
