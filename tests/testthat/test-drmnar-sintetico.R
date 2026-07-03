# Tests de la API pública de estimación y diagnóstico DR-MNAR sobre datos
# sintéticos con mecanismo de respuesta conocido:
#   estimar_drmnar()            -> comparación de 7 estimadores (Figura 1)
#   diagnosticar_norespuesta()  -> tibble de gamma por pregunta/subconjunto
#   resumen_decision_norespuesta() -> regla DR-MNAR vs Raking (Sección 5)

MODELOS_ESPERADOS <- c(
  "Observado", "Weights-MAR", "Imput-MAR", "DR-MAR",
  "Weights-MNAR", "Imput-MNAR", "DR-MNAR"
)

test_that("estimar_drmnar devuelve los 7 estimadores con el esquema esperado", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 0, semilla = 1)

  res <- estimar_drmnar(
    diseno = sint$diseno,
    pregunta = "conoce_cand",
    covariables = "x",
    categoria = "Sí lo conoce",
    instrumento = "drmnar_z"
  )

  expect_s3_class(res, "tbl_df")
  expect_setequal(res$modelo, MODELOS_ESPERADOS)
  expect_true(all(c(
    "modelo", "est", "ee", "inf", "sup",
    "gamma_y", "ee_gamma_y", "pregunta", "categoria", "subconjunto"
  ) %in% names(res)))
  expect_equal(unique(res$pregunta), "conoce_cand")
  expect_equal(unique(res$categoria), "Sí lo conoce")
  # los modelos MAR no estiman gamma
  expect_true(all(is.na(res$gamma_y[res$modelo %in%
                                      c("Observado", "Weights-MAR", "Imput-MAR", "DR-MAR")])))
  # los modelos MNAR sí
  expect_true(all(!is.na(res$gamma_y[res$modelo %in%
                                       c("Weights-MNAR", "Imput-MNAR", "DR-MNAR")])))
  # estimaciones e intervalos válidos
  expect_true(all(res$est >= 0 & res$est <= 1))
  expect_true(all(res$inf <= res$est & res$est <= res$sup))
})

test_that("Weights-MAR coincide con la media ponderada del diseño (svymean+rake)", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 0, semilla = 2,
                                 w = runif(6000, 0.5, 2))
  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  d <- sint$datos
  y <- as.numeric(d$conoce_cand == "Sí lo conoce")
  obs <- !is.na(d$conoce_cand)
  media_svy <- stats::weighted.mean(y[obs], d$peso[obs])
  expect_equal(
    res$est[res$modelo == "Weights-MAR"], media_svy, tolerance = 1e-8
  )
  # y "Observado" es la media SIN pesos de los respondientes
  expect_equal(
    res$est[res$modelo == "Observado"], mean(y[obs]), tolerance = 1e-8
  )
})

test_that("con gamma=0 el diagnóstico concluye MAR y decide Raking", {
  sint <- crear_diseno_sintetico(n = 12000, gamma_y = 0, semilla = 3)

  diag <- diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z"
  )

  expect_s3_class(diag, "tbl_df")
  expect_true(all(c(
    "pregunta", "categoria", "subconjunto", "gamma_y", "ee",
    "inf", "sup", "z_stat", "no_ignorable", "decision"
  ) %in% names(diag)))
  expect_false(diag$no_ignorable[1])
  expect_equal(diag$decision[1], "Raking")
  # el IC de gamma cubre el 0
  expect_true(diag$inf[1] < 0 & diag$sup[1] > 0)
})

test_that("con gamma=2 el diagnóstico detecta MNAR, decide DR-MNAR y corrige", {
  sint <- crear_diseno_sintetico(n = 12000, gamma_y = 2, semilla = 4)

  diag <- diagnosticar_norespuesta(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z"
  )
  expect_true(diag$no_ignorable[1])
  expect_equal(diag$decision[1], "DR-MNAR")
  expect_gt(diag$inf[1], 0) # IC de gamma excluye el 0 por la derecha

  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  err_dr <- abs(res$est[res$modelo == "DR-MNAR"] - sint$media_verdadera)
  err_obs <- abs(res$est[res$modelo == "Observado"] - sint$media_verdadera)
  err_rake <- abs(res$est[res$modelo == "Weights-MAR"] - sint$media_verdadera)
  # la corrección MNAR acerca la estimación a la verdad frente al flujo actual
  expect_lt(err_dr, err_obs / 2)
  expect_lt(err_dr, err_rake / 2)
})

test_that("doble robustez: DR consistente si falla el modelo de imputación", {
  # DGP: y depende de la interacción x1*x2 (omitida del modelo de
  # imputación); la propensión de respuesta sí está bien especificada
  # => IPW correcto => DR consistente aunque la imputación esté mal.
  set.seed(31)
  n <- 40000
  x1 <- rbinom(n, 1, 0.5)
  x2 <- rbinom(n, 1, 0.5)
  z <- rbinom(n, 1, 0.5)
  y_verdadera <- rbinom(n, 1, stats::plogis(-0.5 + 2 * x1 * x2))
  r <- rbinom(n, 1, stats::plogis(-0.25 + 1.5 * z + 0.4 * x1 - 0.3 * x2 +
                                    1.5 * y_verdadera))
  datos <- data.frame(
    drmnar_z = z, x1 = x1, x2 = x2,
    conoce_cand = ifelse(r == 1, ifelse(y_verdadera == 1, "Sí", "No"), NA),
    peso = 1, upm = seq_len(n)
  )
  diseno <- survey::svydesign(ids = ~upm, weights = ~peso, data = datos)

  res <- estimar_drmnar(
    diseno = diseno, pregunta = "conoce_cand", covariables = c("x1", "x2"),
    categoria = "Sí", instrumento = "drmnar_z"
  )
  expect_lt(
    abs(res$est[res$modelo == "DR-MNAR"] - mean(y_verdadera)), 0.03
  )
})

test_that("doble robustez: DR consistente si falla el modelo de propensión", {
  # DGP: la propensión depende de x1*x2 (omitida del modelo de propensión);
  # el modelo de resultado sí está bien especificado
  # => imputación correcta => DR consistente aunque la propensión esté mal.
  set.seed(32)
  n <- 40000
  x1 <- rbinom(n, 1, 0.5)
  x2 <- rbinom(n, 1, 0.5)
  z <- rbinom(n, 1, 0.5)
  y_verdadera <- rbinom(n, 1, stats::plogis(-0.2 + 0.6 * x1 - 0.4 * x2))
  r <- rbinom(n, 1, stats::plogis(-0.5 + 1.5 * z + 1.5 * x1 * x2 +
                                    1.5 * y_verdadera))
  datos <- data.frame(
    drmnar_z = z, x1 = x1, x2 = x2,
    conoce_cand = ifelse(r == 1, ifelse(y_verdadera == 1, "Sí", "No"), NA),
    peso = 1, upm = seq_len(n)
  )
  diseno <- survey::svydesign(ids = ~upm, weights = ~peso, data = datos)

  res <- estimar_drmnar(
    diseno = diseno, pregunta = "conoce_cand", covariables = c("x1", "x2"),
    categoria = "Sí", instrumento = "drmnar_z"
  )
  expect_lt(
    abs(res$est[res$modelo == "DR-MNAR"] - mean(y_verdadera)), 0.03
  )
})

test_that("estimar_drmnar valida instrumento y categoría", {
  sint <- crear_diseno_sintetico(n = 2000, gamma_y = 0, semilla = 5)
  # instrumento inexistente
  expect_error(
    estimar_drmnar(
      diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
      categoria = "Sí lo conoce", instrumento = "no_existe"
    ),
    regexp = "instrumento"
  )
  # variable categórica sin categoría objetivo
  expect_error(
    estimar_drmnar(
      diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
      instrumento = "drmnar_z"
    ),
    regexp = "categoria|categoría"
  )
})

test_that("el subconjunto se estima y se etiqueta", {
  sint <- crear_diseno_sintetico(n = 12000, gamma_y = 2, semilla = 6)
  filtro <- sint$datos$x == 1

  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = NULL,
    categoria = "Sí lo conoce", instrumento = "drmnar_z",
    subconjunto = filtro, nombre_subconjunto = "x = 1"
  )
  expect_equal(unique(res$subconjunto), "x = 1")
  media_grupo <- mean(sint$y_verdadera[filtro])
  expect_lt(
    abs(res$est[res$modelo == "DR-MNAR"] - media_grupo), 0.06
  )
})

test_that("resumen_decision_norespuesta agrega la decisión por pregunta", {
  diag <- tibble::tibble(
    pregunta = c("p1", "p1", "p2"),
    categoria = c("A", "B", "A"),
    subconjunto = "Estado",
    gamma_y = c(0.1, 2.5, 0.05),
    ee = c(0.3, 0.4, 0.2),
    inf = c(-0.49, 1.72, -0.34),
    sup = c(0.69, 3.28, 0.44),
    z_stat = c(0.33, 6.25, 0.25),
    no_ignorable = c(FALSE, TRUE, FALSE),
    decision = c("Raking", "DR-MNAR", "Raking")
  )
  resumen <- resumen_decision_norespuesta(diag)
  expect_equal(nrow(resumen), 2)
  expect_equal(
    resumen$decision[resumen$pregunta == "p1"], "DR-MNAR"
  )
  expect_equal(
    resumen$decision[resumen$pregunta == "p2"], "Raking"
  )
})
