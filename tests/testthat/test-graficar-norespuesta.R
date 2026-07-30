# Gráficas diagnóstico de no respuesta no ignorable y clase R6 NoRespuesta.

diagnostico_fixture <- function() {
  tibble::tibble(
    pregunta = rep(c("conoce_cruz", "conoce_andrea"), each = 2),
    categoria = "Sí lo conoce",
    subconjunto = rep(c("Estado", "Juárez"), 2),
    gamma_y = c(2.1, 1.8, 0.2, -0.1),
    ee = c(0.4, 0.6, 0.3, 0.5),
    inf = c(1.32, 0.62, -0.39, -1.08),
    sup = c(2.88, 2.98, 0.79, 0.88),
    z_stat = c(5.25, 3.0, 0.67, -0.2),
    no_ignorable = c(TRUE, TRUE, FALSE, FALSE),
    decision = c("DR-MNAR", "DR-MNAR", "Raking", "Raking")
  )
}

test_that("graficar_diagnostico_norespuesta produce el caterpillar de gamma", {
  g <- graficar_diagnostico_norespuesta(diagnostico_fixture())
  expect_s3_class(g, "ggplot")
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  # intervalo de gamma + línea de referencia en 0
  expect_true("GeomPointrange" %in% geoms)
  expect_true(any(c("GeomHline", "GeomVline") %in% geoms))
})

test_that("graficar_comparacion_estimadores dibuja los 7 modelos", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 2, semilla = 41)
  res <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  g <- graficar_comparacion_estimadores(res)
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$data), 7)
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPointrange" %in% geoms)
})

test_that("graficar_flujo_norespuesta y graficar_pesos_drmnar devuelven ggplots", {
  bd <- tibble::tibble(
    drmnar_z = c(0, 0, 1, 1, 1, 1),
    drmnar_tema = c(NA, NA, "Política", "Deportes", "Salud", "Política"),
    drmnar_r = c(1, 1, 1, 0, 0, 1)
  )
  flujo <- resumen_flujo_norespuesta(bd)
  g1 <- graficar_flujo_norespuesta(flujo)
  expect_s3_class(g1, "ggplot")

  set.seed(5)
  g2 <- graficar_pesos_drmnar(1 / runif(300, 0.2, 0.9))
  expect_s3_class(g2, "ggplot")
})

test_that("la clase NoRespuesta orquesta diagnóstico, estimación y gráficas", {
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 42)

  nr <- NoRespuesta$new(
    encuesta = NULL,
    diseno = sint$diseno,
    diccionario = NULL,
    tema = tema_morant(),
    covariables = "x",
    instrumento = "drmnar_z"
  )

  diag <- nr$diagnostico(preguntas = list("conoce_cand" = "Sí lo conoce"))
  expect_s3_class(diag, "tbl_df")
  expect_equal(diag$decision[1], "DR-MNAR")

  # el diagnóstico queda cacheado para las gráficas
  g <- nr$grafica_diagnostico()
  expect_s3_class(g, "ggplot")

  est <- nr$estimacion(pregunta = "conoce_cand", categoria = "Sí lo conoce")
  expect_equal(nrow(est), 7)

  g2 <- nr$grafica_comparacion(
    pregunta = "conoce_cand", categoria = "Sí lo conoce"
  )
  expect_s3_class(g2, "ggplot")

  flujo <- nr$flujo()
  expect_s3_class(flujo, "tbl_df")
  expect_gt(nrow(flujo), 0)

  desc <- nr$descriptivos()
  expect_true(all(c("indicador", "valor") %in% names(desc)))
})

test_that("graficar_tabla_covariables arma la lámina de covariables", {
  doc <- data.frame(
    covariable = c("sexo", "rango_edad"),
    tipo = c("Individual", "Individual"),
    fuente = c("Cuestionario", "Cuestionario"),
    mide = c("Sexo (F/M)", "Edad en cortes INEGI")
  )
  # vector enriquecido activo (n >= umbral): la nota lo debe decir
  g <- graficar_tabla_covariables(doc, n_ef = 2157, umbral = 1200,
                                  ricas = c("sexo", "rango_edad", "esc_ctx"))
  expect_s3_class(g, "ggplot")
  expect_match(g$labels$caption, "ENRIQUECIDO")

  # fallback (n < umbral): la nota anuncia el vector que se activaría
  g2 <- graficar_tabla_covariables(doc, n_ef = 800, umbral = 1200,
                                   ricas = c("sexo", "esc_ctx"))
  expect_match(g2$labels$caption, "FALLBACK")

  # sin covariables: no debe reventar
  vacio <- doc[0, ]
  expect_s3_class(
    graficar_tabla_covariables(vacio, n_ef = 800, umbral = 1200, ricas = "sexo"),
    "ggplot")
})
