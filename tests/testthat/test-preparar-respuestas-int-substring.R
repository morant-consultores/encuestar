# Regresión: preparar_respuestas() NO debe tirar variables de cuestionario que
# contienen la subcadena "int" (internet, conoce_interurbano, op_interurbano).
# Antes se usaba `select(-contains("INT"))`, coincidencia por subcadena e
# insensible a mayúsculas, que las eliminaba y las dejaba 100% NULL en el
# snapshot. El drop correcto es un `matches("^INT_?[0-9]+$")` anclado que solo
# toca las columnas numeradas del roster de intentos.

test_that("preparar_respuestas conserva variables de cuestionario con 'int' y tira solo INT numeradas", {
  # Subclase mínima que evita el constructor complejo.
  PreprocesoSoloPreparar <- R6::R6Class(
    inherit = encuestar::Preproceso,
    public = list(
      initialize = function(bd_respuestas) {
        self$bd_respuestas <- bd_respuestas
      }
    )
  )

  bd <- data.frame(
    Id                 = 1:2,
    finalizar          = c("Finalizar", NA),
    cluster            = c(10, 11),
    INT1               = c("Abrieron", "No abrieron"),
    INT2               = c("Aceptaron", NA),
    internet           = c("Si tiene", "No tiene"),
    conoce_interurbano = c("Si", "No"),
    op_interurbano     = c("Buena", NA),
    intentos_entrevistas = c("x", "y"),
    stringsAsFactors   = FALSE
  )

  obj <- PreprocesoSoloPreparar$new(bd)
  obj$preparar_respuestas()
  cols <- names(obj$bd_respuestas_preparadas)

  # Las variables de cuestionario sobreviven con su valor.
  expect_true(all(c("internet", "conoce_interurbano", "op_interurbano") %in% cols))
  expect_equal(obj$bd_respuestas_preparadas$internet, c("Si tiene", "No tiene"))
  expect_equal(obj$bd_respuestas_preparadas$conoce_interurbano, c("Si", "No"))

  # Las columnas numeradas del roster se re-adjuntan (vienen de cols_intento).
  expect_true(all(c("INT1", "INT2") %in% cols))

  # `intentos_entrevistas` sí debe eliminarse (patrón "intentos_").
  expect_false("intentos_entrevistas" %in% cols)

  # TipoRegistro derivado correctamente.
  expect_equal(obj$bd_respuestas_preparadas$TipoRegistro, c("Efectivo", "Otro"))
})
