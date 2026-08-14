# Contraste con los desertores del filtro temático (continuum of resistance).

test_that("el signo del contraste coincide con el signo de gamma simulado", {
  # gamma_y = 2 => quienes tienen y = 1 responden MÁS => entre los que
  # eligieron el módulo hay MÁS "Sí lo conoce" que entre los desertores
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 77)
  bd <- sint$datos
  # los desertores del sintético traen y = 0 forzado; se repone la verdad
  bd$conoce_cand <- ifelse(sint$y_verdadera == 1, "Sí lo conoce", "No lo conoce")

  tab <- comparar_desertores_norespuesta(
    bd, preguntas = list("conoce_cand" = "Sí lo conoce")
  )
  expect_equal(nrow(tab), 1)
  expect_gt(tab$diferencia, 0)
  expect_true(is.finite(tab$p_valor))
  expect_equal(tab$n_voluntario + tab$n_desertor, sum(bd$drmnar_z == 1))
})

test_that("sin desertores devuelve cero filas con las columnas declaradas", {
  bd <- data.frame(
    drmnar_z = c(1, 1, 0), drmnar_r = c(1, 1, 1),
    conoce_cand = c("Sí lo conoce", "No lo conoce", "Sí lo conoce")
  )
  tab <- comparar_desertores_norespuesta(
    bd, preguntas = list("conoce_cand" = "Sí lo conoce")
  )
  expect_equal(nrow(tab), 0)
  expect_true(all(c("pregunta", "diferencia", "p_valor") %in% names(tab)))
})

test_that("graficar_desertores_norespuesta marca coincidencia con gamma", {
  tab <- tibble::tibble(
    pregunta = c("a", "b"), categoria = "Sí",
    n_voluntario = 100L, n_desertor = 400L,
    p_voluntario = c(0.60, 0.30), p_desertor = c(0.45, 0.40),
    diferencia = c(0.15, -0.10), p_valor = c(0.01, 0.20)
  )
  diag <- tibble::tibble(
    pregunta = c("a", "b"), categoria = "Sí",
    gamma_y = c(0.9, 0.8), decision = "DR-MNAR"
  )
  g <- graficar_desertores_norespuesta(tab, diag)
  expect_s3_class(g, "ggplot")
  expect_true("valida" %in% names(g$data))
  # g$data viene en formato largo: dos filas por pregunta (un punto por grupo)
  expect_equal(nrow(g$data), 4)
  # "a" coincide en signo con gamma; "b" no
  expect_true(all(g$data$valida[g$data$pregunta == "a"]))
  expect_false(any(g$data$valida[g$data$pregunta == "b"]))
})

test_that("graficar_desertores_norespuesta devuelve NULL con tabla vacía", {
  vacia <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    n_voluntario = integer(0), n_desertor = integer(0),
    p_voluntario = numeric(0), p_desertor = numeric(0),
    diferencia = numeric(0), p_valor = numeric(0)
  )
  expect_null(graficar_desertores_norespuesta(vacia))
})
