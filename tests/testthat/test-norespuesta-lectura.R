# Lectura de gamma en lenguaje de negocio para las láminas del deck.

fixture_lectura <- function() {
  tibble::tibble(
    pregunta = c("aprob_pm", "calif_horacio", "amai_autos", "amai_cuartos"),
    categoria = c("Aprueba mucho", "0", "1", NA_character_),
    subconjunto = "Estado",
    gamma_y = c(0.92, -3.12, 0.20, NA_real_),
    ee = c(0.30, 1.06, 0.30, NA_real_),
    inf = c(0.33, -5.19, -0.39, NA_real_),
    sup = c(1.52, -1.05, 0.79, NA_real_),
    z_stat = c(3.07, -2.95, 0.67, NA_real_),
    no_ignorable = c(TRUE, TRUE, FALSE, NA),
    decision = c("DR-MNAR", "DR-MNAR", "Raking", "Sin estimación"),
    convergencia = c(TRUE, TRUE, TRUE, FALSE),
    est_rake = c(0.65, 0.10, 0.30, NA_real_),
    est_drmnar = c(0.55, 0.18, 0.30, NA_real_)
  )
}

test_that("gamma positivo significativo se lee como sobre-representación", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "aprob_pm", ]
  expect_equal(fila$estado, "sobre_representacion")
  # el verbo va en mayúsculas a propósito: es texto de lámina
  expect_match(fila$texto, "sobreestima", ignore.case = TRUE)
  # el corrimiento en puntos porcentuales entra en el texto
  expect_match(fila$texto, "10")
})

test_that("gamma negativo significativo se lee como ocultamiento", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "calif_horacio", ]
  expect_equal(fila$estado, "sub_representacion")
  expect_match(fila$texto, "subestima", ignore.case = TRUE)
})

test_that("IC que incluye 0 se lee como ignorable y recomienda raking", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "amai_autos", ]
  expect_equal(fila$estado, "ignorable")
  expect_match(fila$texto, "raking|eficiente")
})

test_that("gamma no estimable o sin convergencia cae en ignorable", {
  lec <- lectura_norespuesta(fixture_lectura())
  expect_equal(lec$estado[lec$pregunta == "amai_cuartos"], "ignorable")
})

test_that("diagnóstico vacío devuelve tibble vacío con las columnas", {
  lec <- lectura_norespuesta(fixture_lectura()[0, ])
  expect_equal(nrow(lec), 0)
  expect_true(all(c("pregunta", "estado", "texto") %in% names(lec)))
})
