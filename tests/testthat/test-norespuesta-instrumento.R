# Balance de covariables por brazo del instrumento (supuesto de exclusión).

test_that("con Z al azar todas las DME caen dentro de la banda", {
  set.seed(11)
  n <- 6000
  bd <- data.frame(
    drmnar_z = stats::rbinom(n, 1, 0.5),
    sexo = sample(c("Hombre", "Mujer"), n, replace = TRUE),
    rango_edad = sample(c("18-24", "25-59", "60+"), n, replace = TRUE)
  )
  tab <- evaluar_instrumento_norespuesta(bd, c("sexo", "rango_edad"))
  expect_true(all(c("covariable", "nivel", "prop_z0", "prop_z1", "dme") %in%
                    names(tab)))
  expect_true(all(abs(tab$dme) < 0.1))
})

test_that("una covariable correlacionada con Z sale fuera de la banda", {
  set.seed(12)
  n <- 6000
  x <- stats::rbinom(n, 1, 0.5)
  bd <- data.frame(
    drmnar_z = stats::rbinom(n, 1, ifelse(x == 1, 0.85, 0.15)),
    sesgada = ifelse(x == 1, "Alto", "Bajo")
  )
  tab <- evaluar_instrumento_norespuesta(bd, "sesgada")
  expect_true(any(abs(tab$dme) > 0.1))
})

test_that("graficar_balance_instrumento dibuja el love plot con su banda", {
  tab <- tibble::tibble(
    covariable = c("sexo", "sexo"), nivel = c("Hombre", "Mujer"),
    prop_z0 = c(0.48, 0.52), prop_z1 = c(0.50, 0.50),
    dme = c(0.04, -0.04)
  )
  g <- graficar_balance_instrumento(tab)
  expect_s3_class(g, "ggplot")
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% geoms)
})

test_that("evaluar_instrumento_norespuesta ignora covariables ausentes", {
  bd <- data.frame(drmnar_z = c(0, 1, 0, 1), sexo = c("H", "M", "H", "M"))
  tab <- evaluar_instrumento_norespuesta(bd, c("sexo", "no_existe"))
  expect_setequal(unique(tab$covariable), "sexo")
})
