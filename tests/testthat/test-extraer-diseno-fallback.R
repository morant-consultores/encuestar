# El fallback silencioso de extraer_diseno() a un diseño SIN conglomerados
# (ids = ~1) subestima los errores estándar (~25% medido en Chihuahua
# Ene-2026: EE de 1.34pp con conglomerados vs 1.01pp sin ellos, con más n)
# porque ignora la correlación intraclase de las entrevistas de una misma
# sección/manzana. Debe ser un ERROR informativo, no un message; el
# analista puede optar conscientemente con permitir_sin_conglomerados.

# MuestraPrueba (subclase sin constructor pesado) vive en helper-muestra.R

respuestas_validas <- function() {
  set.seed(1)
  tibble::tibble(
    strata_1 = rep(c("a", "b"), each = 10),
    cluster_1 = rep(1:4, each = 5),
    cluster_0 = 1:20,
    fpc_1 = rep(c(0.5, 0.5, 0.4, 0.4), each = 5),
    fpc_0 = 0.25,
    y = rnorm(20)
  )
}

test_that("con insumos válidos construye el diseño CON conglomerados", {
  m <- MuestraPrueba$new()
  expect_no_error(
    m$extraer_diseno(
      respuestas = respuestas_validas(), marco_muestral = NULL,
      tipo_encuesta = "ine", sin_peso = FALSE, rake = FALSE
    )
  )
  # la UPM quedó declarada (no ids = ~1)
  expect_gt(ncol(m$diseno$cluster), 0)
  expect_lt(length(unique(m$diseno$cluster[[1]])), nrow(respuestas_validas()))
})

test_that("si el diseño con conglomerados falla, es ERROR (no fallback silencioso)", {
  respuestas_rotas <- respuestas_validas()
  respuestas_rotas$fpc_1[1:5] <- NA # el modo de fallo real: joins vacíos -> NA en fpc

  m <- MuestraPrueba$new()
  expect_error(
    m$extraer_diseno(
      respuestas = respuestas_rotas, marco_muestral = NULL,
      tipo_encuesta = "ine", sin_peso = FALSE, rake = FALSE
    ),
    regexp = "conglomerados"
  )
})

test_that("permitir_sin_conglomerados = TRUE es opt-in consciente con warning", {
  respuestas_rotas <- respuestas_validas()
  respuestas_rotas$fpc_1[1:5] <- NA

  m <- MuestraPrueba$new()
  # svydesign también emite su propio warning informativo al degradar;
  # se capturan todos y se verifica el nuestro
  warns <- testthat::capture_warnings(
    m$extraer_diseno(
      respuestas = respuestas_rotas, marco_muestral = NULL,
      tipo_encuesta = "ine", sin_peso = FALSE, rake = FALSE,
      permitir_sin_conglomerados = TRUE
    )
  )
  expect_true(any(grepl("subestimados", warns)))
  # el diseño degradado existe (estratificado, sin conglomerados)
  expect_s3_class(m$diseno, "survey.design")
})
