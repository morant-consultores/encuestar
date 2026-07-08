# Subclase de Muestra para pruebas: evita el constructor pesado
# (recalcular_fpc sobre el objeto muestra completo) y permite ejercer
# extraer_diseno() directamente con insumos sintéticos.
# Mismo patrón que PreprocesoPrueba (helper-preproceso.R).
MuestraPrueba <- R6::R6Class(
  classname = "MuestraPrueba",
  inherit = encuestar:::Muestra,
  public = list(
    initialize = function() invisible(self)
  )
)
