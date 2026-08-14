# Insumos de la estimación DR-MNAR: candados sobre lo que entra al núcleo.

test_that("pesos de longitud distinta a los datos fallan con un mensaje claro", {
  # Reproduce el modo de falla real: `stats::weights()` devolvía numeric(0)
  # cuando el namespace de survey no estaba cargado (weights.survey.design no
  # quedaba registrado y la llamada caía a weights.default -> NULL). El núcleo
  # recibía w vacío, el glm interno se quedaba sin filas y reventaba con
  # "argument mu must be a non-empty numeric vector", 20 llamadas más abajo.
  #
  # Ese error no decía nada del peso, así que el deck de diagnóstico llevaba
  # semanas perdiendo láminas en silencio. Aquí se exige que falle arriba y
  # diciendo qué pasó.
  sint <- crear_diseno_sintetico(n = 500, gamma_y = 1, semilla = 8)
  roto <- sint$diseno
  roto$prob <- NULL   # => weights.survey.design devuelve numeric(0)

  expect_equal(length(stats::weights(roto)), 0)
  expect_error(
    encuestar:::extraer_insumos_drmnar(
      diseno = roto, pregunta = "conoce_cand", covariables = "x",
      instrumento = "drmnar_z", respuesta_ind = NULL,
      categoria = "Sí lo conoce", subconjunto = NULL
    ),
    "peso"
  )
})

test_that("con pesos completos los insumos salen con la longitud de los datos", {
  sint <- crear_diseno_sintetico(n = 500, gamma_y = 1, semilla = 8)
  ins <- encuestar:::extraer_insumos_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    instrumento = "drmnar_z", respuesta_ind = NULL,
    categoria = "Sí lo conoce", subconjunto = NULL
  )
  expect_equal(length(ins$w), 500)
  expect_equal(length(ins$z), 500)
  expect_equal(nrow(ins$X), 500)
  expect_true(all(is.finite(ins$w)))
})

test_that("cargar encuestar deja registrado weights.survey.design", {
  # La causa raíz: encuestar declaraba survey en Imports del DESCRIPTION pero no
  # importaba NADA de él en el NAMESPACE, así que su namespace no se cargaba y
  # el método S3 de weights nunca quedaba registrado.
  expect_true("survey" %in% loadedNamespaces())
  sint <- crear_diseno_sintetico(n = 200, gamma_y = 0, semilla = 9)
  expect_equal(length(stats::weights(sint$diseno)), 200)
})
