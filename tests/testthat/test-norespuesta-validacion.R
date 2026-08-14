# Cálculo de las validaciones del diagnóstico DR-MNAR.

fixture_multiplicidad <- function() {
  tibble::tibble(
    pregunta = paste0("p", 1:6),
    categoria = "Sí",
    subconjunto = "Estado",
    gamma_y = c(3.1, 2.2, 2.0, 0.4, 0.2, NA_real_),
    z_stat = c(5.0, 2.6, 2.1, 0.9, 0.4, NA_real_),
    no_ignorable = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
    decision = c(rep("DR-MNAR", 3), rep("Raking", 2), "Sin estimación")
  )
}

test_that("ajustar_multiplicidad_norespuesta reproduce stats::p.adjust", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  finitos <- is.finite(tab$z_stat)
  esperado <- stats::p.adjust(tab$p_valor[finitos], method = "BH")
  expect_equal(tab$p_ajustado[finitos], esperado)
  expect_true(all(is.na(tab$p_ajustado[!finitos])))
  expect_equal(attr(tab, "metodo"), "BH")
})

test_that("el ajuste nunca marca más que el diagnóstico crudo", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  expect_lte(sum(tab$sobrevive, na.rm = TRUE),
             sum(fixture_multiplicidad()$no_ignorable, na.rm = TRUE))
})

test_that("Holm es al menos tan estricto como BH", {
  bh <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  holm <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "holm")
  expect_lte(sum(holm$sobrevive, na.rm = TRUE), sum(bh$sobrevive, na.rm = TRUE))
})

test_that("graficar_multiplicidad_norespuesta compara crudo contra ajustado", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  g <- graficar_multiplicidad_norespuesta(tab)
  expect_s3_class(g, "ggplot")
  expect_match(g$labels$subtitle, "azar")
})
