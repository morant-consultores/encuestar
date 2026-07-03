# Covariables contextuales por sección electoral para el modelo DR-MNAR
# (Sección 4 del Anexo Técnico): censo INEGI (escolaridad, conectividad,
# urbanización) + histórico electoral (Margen de Victoria Neto y
# participación promedio).

# fixture electoral con el formato de mex.rda$info$bd: ele_<partido>_<eleccion>
electoral_fixture <- function() {
  tibble::tibble(
    seccion = c("08_0001", "08_0002", "08_0003"),
    # pr_24: oficialista = morena + pt_morena; opositor = pan + pan_pri
    ele_morena_pr_24 = c(100, 40, 0),
    ele_pt_morena_pr_24 = c(20, 10, 0),
    ele_pan_pr_24 = c(60, 80, 50),
    ele_pan_pri_pr_24 = c(20, 10, 30),
    ele_nulos_pr_24 = c(20, 10, 20),
    ele_total_pr_24 = c(220, 150, 100),
    ele_nominal_pr_24 = c(440, 300, 400),
    # gb_23: empate en la sección 1
    ele_morena_gb_23 = c(50, 30, 10),
    ele_pan_gb_23 = c(50, 60, 70),
    ele_total_gb_23 = c(110, 100, 90),
    ele_nominal_gb_23 = c(440, 250, 360)
  )
}

censo_fixture <- function() {
  tibble::tibble(
    entidad = "08",
    seccion = c("08_0001", "08_0002", "08_0003"),
    tipo = c(2, 3, 4),
    pobtot = c(1000, 2000, 500),
    p_18ymas = c(700, 1500, 320),
    graproes = c(9.5, 11.2, 6.1),
    vph_inter = c(150, 480, 20),
    vivpar_hab = c(300, 600, 100)
  )
}

test_that("calcular_margen_victoria_neto usa bloques y coaliciones correctamente", {
  mvn <- calcular_margen_victoria_neto(
    electoral_fixture(),
    elecciones = c("pr_24", "gb_23")
  )
  expect_true(all(c("seccion", "margen_victoria_neto") %in% names(mvn)))
  expect_equal(nrow(mvn), 3)

  # sección 1 / pr_24: oficialista = 100 + 20 = 120; opositor = 60 + 20 = 80
  #   (los nulos NO cuentan en ningún bloque) => (120 - 80) / 220
  # sección 1 / gb_23: (50 - 50) / 110 = 0
  esperado_s1 <- mean(c((120 - 80) / 220, 0))
  expect_equal(
    mvn$margen_victoria_neto[mvn$seccion == "08_0001"], esperado_s1
  )

  # sección 3: bastión opositor => margen claramente negativo
  esperado_s3 <- mean(c((0 - 80) / 100, (10 - 70) / 90))
  expect_equal(
    mvn$margen_victoria_neto[mvn$seccion == "08_0003"], esperado_s3
  )
})

test_that("calcular_margen_victoria_neto solo promedia las elecciones pedidas", {
  mvn <- calcular_margen_victoria_neto(electoral_fixture(), elecciones = "pr_24")
  expect_equal(
    mvn$margen_victoria_neto[mvn$seccion == "08_0001"], (120 - 80) / 220
  )
  # elección inexistente => error claro
  expect_error(
    calcular_margen_victoria_neto(electoral_fixture(), elecciones = "pr_30"),
    regexp = "pr_30"
  )
})

test_that("construir_covariables_seccion integra censo y electoral", {
  cov <- construir_covariables_seccion(
    censo_seccion = censo_fixture(),
    electoral_bd = electoral_fixture(),
    elecciones = c("pr_24", "gb_23")
  )
  expect_true(all(c(
    "seccion", "escolaridad_prom", "pct_conectividad", "tipo_seccion",
    "participacion_prom", "margen_victoria_neto"
  ) %in% names(cov)))
  expect_equal(nrow(cov), 3)

  s1 <- cov[cov$seccion == "08_0001", ]
  expect_equal(s1$escolaridad_prom, 9.5)
  expect_equal(s1$pct_conectividad, 150 / 300)
  # participación promedio: (220/440 + 110/440) / 2
  expect_equal(s1$participacion_prom, mean(c(220 / 440, 110 / 440)))
  expect_s3_class(cov$tipo_seccion, "factor")
})

test_that("construir_covariables_seccion acepta índice de marginación externo", {
  marginacion <- tibble::tibble(
    seccion = c("08_0001", "08_0002"),
    indice_marginacion = c(-0.5, 1.2)
  )
  cov <- construir_covariables_seccion(
    censo_seccion = censo_fixture(),
    marginacion = marginacion
  )
  expect_equal(cov$indice_marginacion[cov$seccion == "08_0001"], -0.5)
  expect_true(is.na(cov$indice_marginacion[cov$seccion == "08_0003"]))
  # sin electoral no hay columnas electorales
  expect_false("margen_victoria_neto" %in% names(cov))
})

test_that("unir_covariables_individuo une por sección normalizando la llave", {
  cov <- construir_covariables_seccion(
    censo_seccion = censo_fixture(),
    electoral_bd = electoral_fixture(),
    elecciones = "pr_24"
  )
  # el snapshot trae SECCION numérica (sin entidad); se normaliza con `entidad`
  respuestas <- tibble::tibble(
    SbjNum = 1:6,
    SECCION = c(1, 1, 2, 3, 3, 3)
  )
  unido <- unir_covariables_individuo(
    respuestas, cov,
    llave = "SECCION", entidad = "08", estandarizar = FALSE
  )
  expect_equal(nrow(unido), 6)
  expect_equal(unido$escolaridad_prom, c(9.5, 9.5, 11.2, 6.1, 6.1, 6.1))
  expect_equal(unido$pct_conectividad[3], 0.8)
})

test_that("unir_covariables_individuo estandariza las covariables continuas", {
  cov <- construir_covariables_seccion(censo_seccion = censo_fixture())
  respuestas <- tibble::tibble(SECCION = c("08_0001", "08_0002", "08_0003"))
  unido <- unir_covariables_individuo(respuestas, cov, estandarizar = TRUE)
  expect_equal(mean(unido$escolaridad_prom), 0, tolerance = 1e-10)
  expect_equal(stats::sd(unido$escolaridad_prom), 1, tolerance = 1e-10)
})

test_that("unir_covariables_individuo avisa de secciones sin covariables", {
  cov <- construir_covariables_seccion(censo_seccion = censo_fixture())
  respuestas <- tibble::tibble(SECCION = c("08_0001", "08_9999"))
  expect_message(
    unido <- unir_covariables_individuo(respuestas, cov, estandarizar = FALSE),
    regexp = "sin covariables"
  )
  expect_true(is.na(unido$escolaridad_prom[2]))
})
