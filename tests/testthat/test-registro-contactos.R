# Registro de contactos por intento (acuerdo metodológico con campo):
# cada renglón del snapshot guarda TODOS sus intentos (tocó y no abrió,
# rechazo, efectiva, ...) en INT_1..INT_15 (o INT1..INT15), porque el
# audio solo se graba durante el cuestionario y auditoría verifica la
# metodología contra ese registro. El pivot largo + la agregación por
# sección alimentan la sobremuestra de la siguiente ola
# (muestreaR::planear_siguiente_ola: seccion, contactos, efectivas).

snapshot_detalle <- function(prefijo = "INT_") {
  bd <- tibble::tibble(
    SbjNum  = 1:5,
    SECCION = c(101L, 101L, 102L, 102L, 103L),
    INT     = c(1L, 3L, 1L, 2L, 2L)
  )
  # detalle por intento (etiquetas de campo)
  bd[[paste0(prefijo, 1)]] <- c("Efectiva", "No abrió", "Efectiva",
                                "Rechazo", "No abrió")
  bd[[paste0(prefijo, 2)]] <- c(NA, "Rechazo", NA, "Efectiva", "Efectiva")
  bd[[paste0(prefijo, 3)]] <- c(NA, "Efectiva", NA, NA, NA)
  for (k in 4:15) bd[[paste0(prefijo, k)]] <- NA_character_
  bd
}

# ---- pivotar_intentos ----

test_that("pivotar_intentos pasa de ancho a largo con un intento por fila", {
  largo <- pivotar_intentos(snapshot_detalle())
  expect_equal(nrow(largo), 9) # 1 + 3 + 1 + 2 + 2 intentos
  expect_true(all(c("SbjNum", "SECCION", "intento", "resultado") %in%
                    names(largo)))
  fila2 <- largo[largo$SbjNum == 2, ]
  expect_equal(fila2$intento, 1:3)
  expect_equal(fila2$resultado, c("No abrió", "Rechazo", "Efectiva"))
})

test_that("pivotar_intentos autodetecta el prefijo INT_ e INT", {
  con_guion <- pivotar_intentos(snapshot_detalle("INT_"))
  sin_guion <- pivotar_intentos(snapshot_detalle("INT"))
  expect_equal(con_guion$resultado, sin_guion$resultado)
  expect_error(
    pivotar_intentos(tibble::tibble(SbjNum = 1, SECCION = 1)),
    "INT"
  )
})

test_that("pivotar_intentos recodifica códigos numéricos con el catálogo y avisa desconocidos", {
  bd <- tibble::tibble(SbjNum = 1:2, SECCION = c(1L, 1L),
                       INT_1 = c(2L, 9L), INT_2 = c(1L, NA))
  catalogo <- c("1" = "Efectiva", "2" = "No abrió")
  expect_warning(
    largo <- pivotar_intentos(bd, codigos = catalogo),
    "9"
  )
  expect_equal(largo$resultado[largo$SbjNum == 1], c("No abrió", "Efectiva"))
  # el código desconocido queda crudo, no NA (no se pierde información)
  expect_equal(largo$resultado[largo$SbjNum == 2], "9")
})

test_that("columnas de intentos presentes pero vacías avisan (caso Chihuahua feb-2026)", {
  bd <- snapshot_detalle()
  for (k in 1:15) bd[[paste0("INT_", k)]] <- NA_integer_
  expect_warning(largo <- pivotar_intentos(bd), "vac")
  expect_equal(nrow(largo), 0)
})

# ---- derivar_registro_contactos ----

test_that("con detalle: contactos, efectivas y desglose por resultado por sección", {
  reg <- derivar_registro_contactos(snapshot_detalle())
  expect_equal(attr(reg, "fuente"), "detalle_intentos")
  # contrato con muestreaR::planear_siguiente_ola
  expect_true(all(c("seccion", "contactos", "efectivas") %in% names(reg)))
  s101 <- reg[reg$seccion == "101", ]
  expect_equal(s101$contactos, 4)      # 1 + 3 intentos
  expect_equal(s101$efectivas, 2)      # 2 renglones (entrevistas)
  expect_equal(s101$intentos_no_abrio, 1)
  expect_equal(s101$intentos_rechazo, 1)
  s103 <- reg[reg$seccion == "103", ]
  expect_equal(s103$contactos, 2)
  expect_equal(s103$intentos_no_abrio, 1)
})

test_that("sin detalle (columnas vacías) cae a la columna INT con aviso", {
  bd <- snapshot_detalle()
  for (k in 1:15) bd[[paste0("INT_", k)]] <- NA_integer_
  expect_warning(reg <- derivar_registro_contactos(bd), "INT")
  expect_equal(attr(reg, "fuente"), "intento_efectivo")
  s101 <- reg[reg$seccion == "101", ]
  expect_equal(s101$contactos, 4)      # INT: 1 + 3
  expect_equal(s101$efectivas, 2)
})

test_that("sin columnas de detalle (esquema sep-2025) también cae a INT", {
  bd <- tibble::tibble(SbjNum = 1:3, SECCION = c(1L, 1L, 2L),
                       INT = c(2L, NA, 15L))
  expect_warning(reg <- derivar_registro_contactos(bd), "INT")
  s1 <- reg[reg$seccion == "1", ]
  expect_equal(s1$contactos, 3)   # 2 + 1 (el NA cuenta como 1 intento: la
  expect_equal(s1$efectivas, 2)   # entrevista existió)
  expect_equal(reg$contactos[reg$seccion == "2"], 15)
})

test_that("valida columnas y falla claro si no hay ni detalle ni INT", {
  expect_error(
    derivar_registro_contactos(tibble::tibble(SbjNum = 1, SECCION = 1)),
    "INT"
  )
  expect_error(
    derivar_registro_contactos(snapshot_detalle(), seccion = "no_existe"),
    "no_existe"
  )
})

# ---- hallazgos del code review ----

test_that("invariante del contrato: contactos >= efectivas aunque haya renglones sin intentos", {
  # adopción gradual de la app: un renglón registró sus intentos, otro no
  bd <- snapshot_detalle()
  bd[2, paste0("INT_", 1:15)] <- NA_character_   # renglón 2 (sección 101) sin registro
  reg <- derivar_registro_contactos(bd)
  s101 <- reg[reg$seccion == "101", ]
  # renglón 1: 1 intento registrado; renglón 2: mínimo 1 (la propia efectiva)
  expect_equal(s101$contactos, 2)
  expect_equal(s101$efectivas, 2)
  expect_true(all(reg$contactos >= reg$efectivas))
})

test_that("una sección cuyos renglones no registraron intentos NO desaparece", {
  bd <- snapshot_detalle()
  bd[5, paste0("INT_", 1:15)] <- NA_character_   # sección 103 queda sin detalle
  reg <- derivar_registro_contactos(bd)
  expect_true("103" %in% reg$seccion)
  s103 <- reg[reg$seccion == "103", ]
  expect_equal(s103$contactos, 1)
  expect_equal(s103$efectivas, 1)
})

test_that("etiquetas que normalizan igual se suman en una sola columna", {
  bd <- tibble::tibble(SbjNum = 1:2, SECCION = c(1L, 1L),
                       INT_1 = c("No abrió", "no abrio"),
                       INT_2 = c("Efectiva", "Efectiva"))
  reg <- derivar_registro_contactos(bd)
  expect_equal(reg$intentos_no_abrio, 2)
  expect_equal(reg$contactos, 4)
})

test_that("el prefijo personalizado también numera bien los intentos", {
  bd <- tibble::tibble(SbjNum = 1L, SECCION = 1L,
                       VISITA_1 = "No abrió", VISITA_2 = "Efectiva")
  largo <- pivotar_intentos(bd, prefijo = "VISITA_", max_intentos = 5)
  expect_equal(largo$intento, 1:2)
})

test_that("secciones NA se excluyen con aviso (fila fantasma rompe aguas abajo)", {
  bd <- snapshot_detalle()
  bd$SECCION[5] <- NA
  expect_warning(reg <- derivar_registro_contactos(bd), "sin sección")
  expect_false(any(is.na(reg$seccion)))
  expect_equal(sum(reg$efectivas), 4)
})

test_that("el aviso de códigos fuera de catálogo sobrevive vía derivar_registro_contactos", {
  bd <- tibble::tibble(SbjNum = 1L, SECCION = 1L, INT_1 = 9L, INT_2 = 1L)
  expect_warning(
    derivar_registro_contactos(bd, codigos = c("1" = "Efectiva")),
    "9"
  )
})

test_that("intentos de puros espacios no cuentan", {
  bd <- tibble::tibble(SbjNum = 1L, SECCION = 1L,
                       INT_1 = "  ", INT_2 = "Efectiva")
  largo <- pivotar_intentos(bd)
  expect_equal(nrow(largo), 1)
})

test_that("el respaldo INT tolera la columna como texto", {
  bd <- tibble::tibble(SbjNum = 1:2, SECCION = c(1L, 1L),
                       INT = c("3", "no numérico"))
  expect_warning(reg <- derivar_registro_contactos(bd), "INT")
  expect_equal(reg$contactos, 4)   # 3 + 1 (lo no numérico cuenta como 1)
})
