# Reducción automática de covariables cuando un subgrupo no las identifica.

# Diseño sintético con una covariable contextual que NO identifica el modelo
# dentro del subgrupo: fuera del subgrupo varía, dentro es casi unánime. Es el
# caso real de las contextuales por sección (terciles) dentro de un municipio
# chico: la celda queda tan esparcida que el logístico no se puede ajustar.
diseno_con_covariable_problematica <- function(n = 4000, semilla = 31) {
  sint <- crear_diseno_sintetico(n = n, gamma_y = 2, semilla = semilla)
  bd <- sint$diseno$variables
  set.seed(semilla)
  bd$region <- rep(c("grande", "chico"), length.out = nrow(bd))
  # contextual: varía en "grande", constante en "chico"
  bd$ctx <- ifelse(bd$region == "grande",
                   sample(c("bajo", "medio", "alto"), nrow(bd), replace = TRUE),
                   "medio")
  sint$diseno$variables <- bd
  sint$sub_chico <- bd$region == "chico"
  sint
}

test_that("sin covariables_auto un subgrupo sin identificación queda Sin estimación", {
  sint <- diseno_con_covariable_problematica()
  out <- suppressWarnings(diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = c("x", "ctx"), instrumento = "drmnar_z",
    subconjuntos = list("chico" = sint$sub_chico)
  ))
  expect_equal(nrow(out), 1)
  expect_equal(out$decision, "Sin estimación")
})

test_that("con covariables_auto retira las que no identifican y estima", {
  sint <- diseno_con_covariable_problematica()
  out <- suppressMessages(diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = c("x", "ctx"), instrumento = "drmnar_z",
    subconjuntos = list("chico" = sint$sub_chico),
    covariables_auto = TRUE
  ))
  expect_equal(nrow(out), 1)
  expect_true(is.finite(out$gamma_y))
  expect_true(out$decision %in% c("DR-MNAR", "Raking"))
})

test_that("covariables_usadas declara el vector real de cada subgrupo", {
  sint <- diseno_con_covariable_problematica()
  out <- suppressMessages(diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = c("x", "ctx"), instrumento = "drmnar_z",
    subconjuntos = list("grande" = !sint$sub_chico, "chico" = sint$sub_chico),
    covariables_auto = TRUE
  ))
  expect_true("covariables_usadas" %in% names(out))
  # "grande" identifica el vector completo; "chico" tuvo que soltar la última
  expect_equal(out$covariables_usadas[out$subconjunto == "grande"], "x + ctx")
  expect_equal(out$covariables_usadas[out$subconjunto == "chico"], "x")
})

test_that("la reducción avisa, no pasa callada", {
  sint <- diseno_con_covariable_problematica()
  expect_message(
    diagnosticar_norespuesta(
      diseno = sint$diseno,
      preguntas = list("conoce_cand" = "Sí lo conoce"),
      covariables = c("x", "ctx"), instrumento = "drmnar_z",
      subconjuntos = list("chico" = sint$sub_chico),
      covariables_auto = TRUE
    ),
    "ctx"
  )
})

test_that("covariables_usadas también sale con covariables_auto apagado", {
  # la columna es parte del contrato de salida, no un extra del modo auto:
  # quien lea la tabla debe poder saber siempre con qué se estimó
  sint <- crear_diseno_sintetico(n = 3000, gamma_y = 2, semilla = 32)
  out <- diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x", instrumento = "drmnar_z"
  )
  expect_equal(out$covariables_usadas, "x")
})
