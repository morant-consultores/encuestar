# Gráficas de validación e impacto del diagnóstico DR-MNAR.

fixture_impacto <- function() {
  tibble::tibble(
    pregunta = c("aprob_pm", "calif_horacio", "amai_autos"),
    categoria = c("Aprueba mucho", "0", "1"),
    subconjunto = "Estado",
    gamma_y = c(0.92, -3.12, 0.20),
    inf = c(0.33, -5.19, -0.39),
    sup = c(1.52, -1.05, 0.79),
    no_ignorable = c(TRUE, TRUE, FALSE),
    decision = c("DR-MNAR", "DR-MNAR", "Raking"),
    est_rake = c(0.65, 0.10, 0.30),
    est_drmnar = c(0.55, 0.18, 0.30),
    diferencia = c(-0.10, 0.08, 0.00)
  )
}

test_that("graficar_impacto_drmnar dibuja solo las preguntas DR-MNAR", {
  g <- graficar_impacto_drmnar(fixture_impacto())
  expect_s3_class(g, "ggplot")
  # 2 preguntas DR-MNAR x 2 estimadores = 4 puntos; amai_autos queda fuera
  expect_equal(nrow(g$data), 4)
  expect_false("amai_autos" %in% g$data$pregunta)
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
})

test_that("graficar_impacto_drmnar devuelve NULL sin filas DR-MNAR", {
  solo_raking <- fixture_impacto()[3, ]
  expect_null(graficar_impacto_drmnar(solo_raking))
})

test_that("graficar_precision_drmnar ordena los 7 estimadores y marca los MNAR", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 2, semilla = 41)
  est <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  g <- graficar_precision_drmnar(est)
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$data), 7)
  expect_true("tipo" %in% names(g$data))
  expect_setequal(unique(g$data$tipo), c("MAR/Observado", "MNAR"))
})

test_that("graficar_precision_drmnar devuelve NULL sin errores estándar finitos", {
  est <- tibble::tibble(
    modelo = "DR-MNAR", est = 0.5, ee = NA_real_,
    pregunta = "x", categoria = "a", subconjunto = "Estado"
  )
  expect_null(graficar_precision_drmnar(est))
})
