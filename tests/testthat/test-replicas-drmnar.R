# Diseño de réplicas DR-MNAR: el weight-swap ingenuo (svydesign con pesos
# diseño x 1/pi_hat) da a svymean un EE que IGNORA la incertidumbre de
# pi_hat — sale demasiado angosto. disenar_replicas_drmnar() devuelve un
# svrepdesign donde CADA réplica re-estima pi_hat sobre un remuestreo de
# UPM, de modo que survey::svymean propaga la varianza completa y el EE se
# acerca al del sándwich de estimar_drmnar().

test_that("disenar_replicas_drmnar devuelve un svrepdesign consumible por svymean", {
  sint <- crear_diseno_sintetico(n = 4000, gamma_y = 1.5, semilla = 51,
                                 n_clusters = 120)
  rep <- disenar_replicas_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z", n_replicas = 80
  )
  expect_s3_class(rep, "svyrep.design")
  vars <- rep$variables
  m <- survey::svymean(
    ~I(conoce_cand == "Sí lo conoce"), rep, na.rm = TRUE
  )
  expect_true(is.finite(survey::SE(m)[1]))
})

test_that("el EE de réplicas es mayor que el del weight-swap ingenuo (captura la incertidumbre de pi)", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 2, semilla = 7,
                                 n_clusters = 150)
  # weight-swap ingenuo
  dis_naive <- ajustar_pesos_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  y <- as.numeric(dis_naive$variables$conoce_cand == "Sí lo conoce")
  ee_naive <- survey::SE(survey::svymean(
    ~y_d, stats::update(dis_naive, y_d = y), na.rm = TRUE
  ))[1]

  # diseño de réplicas
  rep <- disenar_replicas_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z", n_replicas = 150
  )
  ee_rep <- survey::SE(survey::svymean(
    ~I(conoce_cand == "Sí lo conoce"), rep, na.rm = TRUE
  ))[1]

  expect_gt(ee_rep, ee_naive)
})

test_that("el EE de réplicas se aproxima al del sándwich de estimar_drmnar", {
  sint <- crear_diseno_sintetico(n = 8000, gamma_y = 2, semilla = 11,
                                 n_clusters = 200)
  est <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  ee_sandwich <- est$ee[est$modelo == "Weights-MNAR"]

  rep <- disenar_replicas_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z", n_replicas = 200
  )
  ee_rep <- survey::SE(survey::svymean(
    ~I(conoce_cand == "Sí lo conoce"), rep, na.rm = TRUE
  ))[1]

  # mismo orden de magnitud (bootstrap vs linealización): +-40%
  expect_lt(abs(ee_rep - ee_sandwich) / ee_sandwich, 0.40)
})

test_that("la media de réplicas coincide con el estimador IPW-MNAR puntual", {
  sint <- crear_diseno_sintetico(n = 5000, gamma_y = 1.5, semilla = 13,
                                 n_clusters = 120)
  est <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  rep <- disenar_replicas_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z", n_replicas = 100
  )
  rep <- stats::update(rep, y_d = as.numeric(rep$variables$conoce_cand == "Sí lo conoce"))
  media_rep <- as.numeric(coef(survey::svymean(~y_d, rep, na.rm = TRUE)))[1]
  expect_lt(abs(media_rep - est$est[est$modelo == "Weights-MNAR"]), 0.02)
})
