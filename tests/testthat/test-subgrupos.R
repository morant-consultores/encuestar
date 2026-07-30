# Diagnóstico DR-MNAR por subconjuntos: puede no haber evidencia de no
# respuesta no ignorable a nivel global y sí haberla por subgrupo (o con
# signos opuestos que se cancelan globalmente).

test_that("detecta MNAR por subgrupo cuando el efecto global se diluye", {
  # un subgrupo minoritario (B, 6%) con no respuesta severamente no
  # ignorable (gamma = 2) dentro de una mayoría MAR (A, gamma = 0):
  # a nivel global el sesgo se diluye y el diagnóstico no rechaza
  # ignorabilidad, pero dentro del grupo B sí. (Semilla y tamaños
  # calibrados empíricamente: z_global ~ 1.4, z_B ~ 3.5.)
  set.seed(202)
  n <- 35000
  grupo <- rbinom(n, 1, 0.06) # 1 = B (MNAR, minoritario)
  x <- rbinom(n, 1, 0.5)
  z <- rbinom(n, 1, 0.5)
  y_verdadera <- rbinom(n, 1, stats::plogis(-0.2 + 0.8 * x))
  gamma_i <- ifelse(grupo == 1, 2, 0)
  r <- rbinom(n, 1, stats::plogis(-0.25 + 1.5 * z + 0.5 * x +
                                    gamma_i * y_verdadera))
  datos <- data.frame(
    drmnar_z = z, x = x, grupo = ifelse(grupo == 1, "B", "A"),
    conoce_cand = ifelse(r == 1, ifelse(y_verdadera == 1, "Sí", "No"), NA),
    peso = 1, upm = seq_len(n)
  )
  diseno <- survey::svydesign(ids = ~upm, weights = ~peso, data = datos)

  diag <- diagnosticar_norespuesta(
    diseno = diseno,
    preguntas = list("conoce_cand" = "Sí"),
    covariables = "x",
    instrumento = "drmnar_z",
    subconjuntos = list(
      "Global" = NULL,
      "Grupo A" = datos$grupo == "A",
      "Grupo B" = datos$grupo == "B"
    )
  )

  expect_equal(nrow(diag), 3)
  expect_setequal(diag$subconjunto, c("Global", "Grupo A", "Grupo B"))

  global <- diag[diag$subconjunto == "Global", ]
  grupo_a <- diag[diag$subconjunto == "Grupo A", ]
  grupo_b <- diag[diag$subconjunto == "Grupo B", ]

  # global: el sesgo se diluye => no rechaza ignorabilidad => Raking
  expect_false(global$no_ignorable)
  expect_equal(global$decision, "Raking")

  # mayoría MAR: Raking
  expect_false(grupo_a$no_ignorable)
  expect_equal(grupo_a$decision, "Raking")

  # subgrupo MNAR: detectado => DR-MNAR
  expect_true(grupo_b$no_ignorable)
  expect_equal(grupo_b$decision, "DR-MNAR")
  expect_gt(grupo_b$inf, 0) # gamma positivo: los Y=1 responden más
})

test_that("un subgrupo sin datos o sin convergencia devuelve NA con aviso", {
  sint <- crear_diseno_sintetico(n = 3000, gamma_y = 0, semilla = 9)
  filtro_vacio <- rep(FALSE, nrow(sint$datos))

  expect_warning(
    diag <- diagnosticar_norespuesta(
      diseno = sint$diseno,
      preguntas = list("conoce_cand" = "Sí lo conoce"),
      covariables = "x",
      instrumento = "drmnar_z",
      subconjuntos = list("Vacío" = filtro_vacio)
    ),
    regexp = "Vacío"
  )
  expect_true(is.na(diag$gamma_y[diag$subconjunto == "Vacío"]))
  expect_equal(diag$decision[diag$subconjunto == "Vacío"], "Sin estimación")
})
