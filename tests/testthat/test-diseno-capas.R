# Arquitectura de pesos por capas (DECISION_ARQUITECTURA_PESOS.md):
#   Capa 1: peso de SELECCIÓN PLANEADA  w1 = LN_s / (pi_s * n_plan)
#           (bajo PPS + cuota fija por sección es ~constante: epsem)
#   Capa 2: clase de NO RESPUESTA por sección  f_s = min(n_plan/logradas_s, tope)
#           (declarada, con tope; reemplaza al fpc empírico MCAR-encubierto)
#   Capa 3: rake demográfico a márgenes poblacionales (con monitoreo)
# El fpc empírico (logradas/población) queda retirado de esta ruta.

plan_fixture <- function() {
  # 4 secciones PPS: pi proporcional a LN (diseño epsem exacto)
  tibble::tibble(
    seccion = c("S1", "S2", "S3", "S4"),
    ln_seccion = c(1000, 2000, 3000, 4000),
    pi_seccion = sampling::inclusionprobabilities(c(1000, 2000, 3000, 4000), 2),
    n_plan = 10
  )
}

bd_fixture <- function(logradas = c(10, 10, 10, 10)) {
  set.seed(1)
  bd <- purrr::map2_dfr(
    plan_fixture()$seccion, logradas,
    ~ tibble::tibble(SECCION = .x, id = paste0(.x, "_", seq_len(.y)))
  )
  n <- nrow(bd)
  bd$sexo <- rep(c("F", "M"), length.out = n)
  bd$rango_edad <- rep(c("18A24", "60YMAS"), length.out = n)
  bd
}

margenes_fixture <- function() {
  list(
    sexo = data.frame(sexo = c("F", "M"), Freq = c(5000, 5000)),
    rango_edad = data.frame(rango_edad = c("18A24", "60YMAS"),
                            Freq = c(5000, 5000))
  )
}

test_that("capa 1: bajo PPS exacto los pesos de selección son constantes (epsem)", {
  res <- construir_diseno_capas(
    bd = bd_fixture(), plan = plan_fixture(), seccion = "SECCION"
  )
  capas <- attr(res, "capas")
  expect_s3_class(res, "survey.design")
  # epsem: w1 identico en las 4 secciones
  expect_lt(diff(range(capas$w_seleccion)), 1e-9)
  # cuota completa: factor de clase 1 en todas
  expect_true(all(capas$factor_clase == 1))
})

test_that("capa 2: el factor de clase es plan/logradas con tope", {
  # S1 completa (10), S2 a la mitad (5), S3 muy corta (2 -> tope), S4 completa
  res <- construir_diseno_capas(
    bd = bd_fixture(logradas = c(10, 5, 2, 10)),
    plan = plan_fixture(), seccion = "SECCION", tope_clase = 3
  )
  capas <- attr(res, "capas")
  expect_equal(capas$factor_clase[capas$seccion == "S1"], 1)
  expect_equal(capas$factor_clase[capas$seccion == "S2"], 2)
  expect_equal(capas$factor_clase[capas$seccion == "S3"], 3) # 10/2 = 5 -> tope 3
  # el peso final incorpora la clase
  w <- stats::weights(res)
  bd <- bd_fixture(logradas = c(10, 5, 2, 10))
  expect_equal(
    mean(w[bd$SECCION == "S2"]) / mean(w[bd$SECCION == "S1"]), 2,
    tolerance = 1e-9
  )
})

test_that("secciones levantadas fuera del plan reciben peso mediano con aviso", {
  bd <- dplyr::bind_rows(
    bd_fixture(),
    tibble::tibble(SECCION = "S_FUERA", id = paste0("f", 1:10),
                   sexo = rep(c("F", "M"), 5),
                   rango_edad = rep(c("18A24", "60YMAS"), 5))
  )
  expect_warning(
    res <- construir_diseno_capas(bd, plan_fixture(), seccion = "SECCION"),
    regexp = "fuera del plan"
  )
  capas <- attr(res, "capas")
  expect_equal(
    capas$w_seleccion[capas$seccion == "S_FUERA"],
    stats::median(capas$w_seleccion[capas$seccion != "S_FUERA"])
  )
})

test_that("capa 3: el rake alcanza los márgenes poblacionales", {
  # campo desbalanceado: más mujeres que hombres
  bd <- bd_fixture()
  bd$sexo <- c(rep("F", 28), rep("M", 12))
  res <- construir_diseno_capas(
    bd, plan_fixture(), seccion = "SECCION",
    margenes = margenes_fixture()
  )
  tot <- survey::svytotal(~sexo, res)
  expect_equal(as.numeric(tot), c(5000, 5000), tolerance = 1e-6)
})

test_that("la UPM declarada es la sección (no individuos)", {
  res <- construir_diseno_capas(bd_fixture(), plan_fixture(), seccion = "SECCION")
  expect_equal(length(unique(res$cluster[[1]])), 4)
})

test_that("deff menor que el esquema de fpc empírico puro en escenario desbalanceado", {
  # fpc empírico: peso propor. a 1/logradas SIN tope -> S3 (2 logradas) explota
  logradas <- c(10, 8, 2, 10)
  bd <- bd_fixture(logradas)
  res <- construir_diseno_capas(bd, plan_fixture(), seccion = "SECCION",
                                tope_clase = 2)
  kish <- function(w) 1 + (stats::sd(w) / mean(w))^2
  w_v2 <- stats::weights(res)
  # el esquema viejo equivaldría a factor 1/tasa sin tope (5x en S3)
  w_viejo <- (10 / logradas)[match(bd$SECCION, plan_fixture()$seccion)]
  expect_lt(kish(w_v2), kish(w_viejo))
})

# ---- SOBRE-EJECUCIÓN: el factor debe DEFLACTAR (regresión) -------------------
# Antes había un pmax(..., 1) que impedía que el factor bajara de 1: una
# sección que levantaba de más no deflactaba y terminaba representando tantas
# veces su población como veces se hubiera excedido.
test_that("capa 2: sobre-ejecutar deflacta (factor < 1, sin piso en 1)", {
  capas <- construir_diseno_capas(
    bd = bd_fixture(logradas = c(10, 20, 50, 10)),
    plan = plan_fixture(), seccion = "SECCION", tope_clase = 3
  ) |> attr("capas")
  expect_equal(capas$factor_clase[capas$seccion == "S1"], 1)     # 10/10
  expect_equal(capas$factor_clase[capas$seccion == "S2"], 0.5)   # 10/20
  expect_equal(capas$factor_clase[capas$seccion == "S3"], 0.2)   # 10/50
})

test_that("el peso TOTAL de una sección no depende de cuántas entrevistas hizo", {
  # La población de una sección es fija: sobre-ejecutar no puede inflarla.
  totales <- vapply(list(c(10, 10, 10, 10), c(10, 20, 50, 10)), function(lg) {
    capas <- construir_diseno_capas(
      bd = bd_fixture(logradas = lg), plan = plan_fixture(),
      seccion = "SECCION", tope_clase = 3
    ) |> attr("capas")
    s3 <- capas[capas$seccion == "S3", ]
    s3$peso_seccion * s3$logradas          # población representada por S3
  }, numeric(1))
  expect_equal(totales[1], totales[2], tolerance = 1e-9)
})

# ---- tope POR SECCIÓN: cerradas sin tope, en campo con tope -----------------
test_that("tope_seccion del plan manda sobre el tope_clase global", {
  plan <- plan_fixture()
  plan$tope_seccion <- c(3, 3, Inf, 3)     # S3 cerrada: no respuesta real
  capas <- construir_diseno_capas(
    bd = bd_fixture(logradas = c(10, 5, 2, 10)),
    plan = plan, seccion = "SECCION", tope_clase = 3
  ) |> attr("capas")
  expect_equal(capas$factor_clase[capas$seccion == "S2"], 2)   # 10/5, bajo tope
  expect_equal(capas$factor_clase[capas$seccion == "S3"], 5)   # 10/2 SIN topar
})
