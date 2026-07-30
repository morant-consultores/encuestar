# Integración del DR-MNAR con el flujo de morantviz:
#   1. Weight-swap: ajustar_pesos_drmnar() devuelve un svydesign cuyos
#      pesos son diseño x 1/pi (propensión MNAR); morantviz lo consume SIN
#      cambios y svymean reproduce el estimador IPW-MNAR.
#   2. Inyección: exportar_resultado_morantviz() emite el tibble con el
#      esquema de salida de morantviz para graficar el DR completo.
#   3. diseno_para_pregunta(): respeta la decisión del bundle (Raking =>
#      diseño original; DR-MNAR => diseño repesado).

test_that("el weight-swap reproduce el estimador IPW-MNAR vía svymean", {
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 51)

  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )

  dis_ajustado <- ajustar_pesos_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  expect_s3_class(dis_ajustado, "survey.design")

  # svymean sobre el diseño repesado (flujo morantviz sin cambios)
  vars <- dis_ajustado$variables
  media_svy <- survey::svymean(
    ~y_dico,
    design = stats::update(dis_ajustado, y_dico = as.numeric(
      vars$conoce_cand == "Sí lo conoce"
    )),
    na.rm = TRUE
  )
  expect_equal(
    as.numeric(coef(media_svy)),
    res$est[res$modelo == "Weights-MNAR"],
    tolerance = 1e-3
  )
})

test_that("los pesos ajustados difieren de los originales solo en respondientes", {
  sint <- crear_diseno_sintetico(n = 5000, gamma_y = 1.5, semilla = 52)
  dis_ajustado <- ajustar_pesos_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  w_orig <- stats::weights(sint$diseno)
  w_nuevo <- stats::weights(dis_ajustado)
  r <- sint$datos$r
  # los respondientes reciben el factor 1/pi > 1
  expect_true(all(w_nuevo[r == 1] > w_orig[r == 1] * 0.999))
  expect_gt(mean(w_nuevo[r == 1] / w_orig[r == 1]), 1.1)
})

test_that("exportar_resultado_morantviz cumple el esquema de salida", {
  sint <- crear_diseno_sintetico(n = 5000, gamma_y = 1.5, semilla = 53)
  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  salida <- exportar_resultado_morantviz(res)
  # esquema que consumen los métodos de morantviz (contar_vars_pesos)
  expect_true(all(c("respuesta", "media", "ee", "inf", "sup", "codigo") %in%
                    names(salida)))
  expect_equal(salida$codigo, "conoce_cand")
  expect_equal(salida$media, res$est[res$modelo == "DR-MNAR"])
  # se puede pedir otro modelo
  salida_rake <- exportar_resultado_morantviz(res, modelo = "Weights-MAR")
  expect_equal(salida_rake$media, res$est[res$modelo == "Weights-MAR"])
})

test_that("diseno_para_pregunta respeta la decisión del bundle", {
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 54)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z"
  )

  # decisión automática: DR-MNAR => pesos repesados
  dis_dr <- diseno_para_pregunta(bundle, "conoce_cand", categoria = "Sí lo conoce")
  expect_false(isTRUE(all.equal(
    stats::weights(dis_dr), stats::weights(sint$diseno)
  )))

  # con override a Raking => diseño original intacto
  bundle_rake <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z",
    override = tibble::tibble(pregunta = "conoce_cand", decision = "Raking")
  )
  dis_rake <- diseno_para_pregunta(bundle_rake, "conoce_cand",
                                   categoria = "Sí lo conoce")
  expect_equal(stats::weights(dis_rake), stats::weights(sint$diseno))

  # pregunta no diagnosticada => error claro
  expect_error(
    diseno_para_pregunta(bundle, "otra_pregunta"),
    regexp = "otra_pregunta"
  )
})
