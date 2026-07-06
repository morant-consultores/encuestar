# Monitoreo de los factores de ajuste del rake.
#
# Al retirar las cuotas de campo (generan sesgo al intentar cumplirlas),
# el rake absorbe TODA la corrección de composición demográfica. El costo
# es varianza: si el campo se desbalancea (p. ej. levantar de día deja
# fuera a hombres jóvenes), los factores de rake crecen y el deff sube.
# extraer_diseno() debe reportar el rango de factores y ALERTAR cuando
# salen del umbral — la señal operativa de que faltan pases de horario.

marco_fixture <- function() {
  # márgenes poblacionales 50/50 sexo y 50/50 rango de edad
  tibble::tibble(
    LN22_18A24_F = 250, LN22_18A24_M = 250,
    LN22_60YMAS_F = 250, LN22_60YMAS_M = 250
  )
}

respuestas_rake <- function(prop_f) {
  set.seed(10)
  n <- 400
  n_f <- round(n * prop_f)
  tibble::tibble(
    strata_1 = rep(c("a", "b"), each = n / 2),
    cluster_1 = rep(1:8, each = n / 8),
    cluster_0 = 1:n,
    fpc_1 = rep(rep(c(0.5, 0.4), each = n / 4), 2)[1:n],
    fpc_0 = 0.25,
    sexo = c(rep("F", n_f), rep("M", n - n_f)),
    rango_edad = rep(c("18A24", "60YMAS"), length.out = n)
  )
}

test_that("con campo balanceado reporta factores sin alertar", {
  m <- MuestraPrueba$new()
  expect_message(
    expect_no_warning(
      m$extraer_diseno(
        respuestas = respuestas_rake(prop_f = 0.5),
        marco_muestral = marco_fixture(),
        tipo_encuesta = "ine", sin_peso = FALSE, rake = TRUE
      )
    ),
    regexp = "[Ff]actores"
  )
  expect_s3_class(m$diseno, "survey.design")
})

test_that("con campo muy desbalanceado ALERTA factores de rake extremos", {
  m <- MuestraPrueba$new()
  # 85% mujeres contra un margen poblacional de 50%: el factor de los
  # hombres se dispara por encima del umbral superior
  expect_warning(
    m$extraer_diseno(
      respuestas = respuestas_rake(prop_f = 0.85),
      marco_muestral = marco_fixture(),
      tipo_encuesta = "ine", sin_peso = FALSE, rake = TRUE
    ),
    regexp = "rake|composición"
  )
})

test_that("el umbral de alerta es configurable", {
  m <- MuestraPrueba$new()
  # con umbral laxo el mismo desbalance no alerta
  expect_no_warning(
    m$extraer_diseno(
      respuestas = respuestas_rake(prop_f = 0.85),
      marco_muestral = marco_fixture(),
      tipo_encuesta = "ine", sin_peso = FALSE, rake = TRUE,
      umbral_rake = c(0.1, 10)
    )
  )
})
