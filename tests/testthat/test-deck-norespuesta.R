# Armado del deck de diagnóstico DR-MNAR desde el paquete.

test_that("armar_deck_norespuesta produce un pptx con las láminas del corte", {
  sint <- crear_diseno_sintetico(n = 7000, gamma_y = 2, semilla = 91)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x", instrumento = "drmnar_z"
  )
  salida <- file.path(tempdir(), "deck_norespuesta.pptx")

  ruta <- armar_deck_norespuesta(
    diseno = sint$diseno, bundle = bundle,
    plantilla = NULL, salida = salida,
    titulo = "Diagnóstico de no respuesta",
    subtitulo = "Prueba sintética",
    layout = "Title and Content", master = "Office Theme"
  )

  expect_true(file.exists(ruta))
  # portada + al menos flujo, balance, gamma, impacto, multiplicidad y decisión
  expect_gte(length(officer::read_pptx(ruta)), 6)
})

test_that("armar_deck_norespuesta omite las láminas sin insumo en vez de reventar", {
  # gamma = 0 => ninguna pregunta activa DR-MNAR => sin impacto ni pesos
  sint <- crear_diseno_sintetico(n = 5000, gamma_y = 0, semilla = 92)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x", instrumento = "drmnar_z"
  )
  salida <- file.path(tempdir(), "deck_norespuesta_vacio.pptx")

  expect_no_error(
    ruta <- armar_deck_norespuesta(
      diseno = sint$diseno, bundle = bundle,
      plantilla = NULL, salida = salida,
      titulo = "Sin detecciones", subtitulo = "Prueba sintética",
      layout = "Title and Content", master = "Office Theme"
    )
  )
  expect_true(file.exists(ruta))
})
