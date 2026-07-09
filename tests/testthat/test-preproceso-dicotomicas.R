# Preparación de variables DR-MNAR al final del preprocesamiento:
# instrumento z, indicador de respuesta por protocolo, dicotómicas por
# pregunta (reservadas para los cálculos), flujo Figure 2, descriptivos
# Table A16 y el bundle `diseno_drmnar` (segundo tipo de diseño).

# fixture estilo snapshot de Chihuahua: gustos_aleatorio_50 NA = brazo
# control (directo a política); no-NA = brazo tratamiento (filtro temático)
snapshot_fixture <- function() {
  tibble::tibble(
    SbjNum = 1:10,
    SECCION = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    gustos_aleatorio_50 = c(
      NA, NA, NA, NA, # control
      "Política", "Política", # tratamiento que eligió política
      "Deportes", "Salud", "Ns/Nc", "Películas" # desertores del filtro
    ),
    conoce_cruz = c(
      "Sí lo conoce", "No lo conoce", "Sí lo conoce", "Sí lo conoce",
      "No lo conoce", "Sí lo conoce",
      # en Chihuahua los desertores sí respondieron (el campo no
      # interrumpió); bajo protocolo su respuesta se enmascara
      "Sí lo conoce", "Sí lo conoce", "No lo conoce", "Sí lo conoce"
    ),
    edad = c(25, 40, 33, 61, 45, 22, 38, 50, 29, 70),
    sexo = c("M", "F", "F", "M", "F", "M", "M", "F", "F", "M")
  )
}

test_that("preparar_variables_drmnar crea z, tema, r y dicotómicas por pregunta", {
  bd <- preparar_variables_drmnar(
    snapshot_fixture(),
    instrumento_campo = "gustos_aleatorio_50",
    opcion_politica = "Política",
    preguntas = list("conoce_cruz" = "Sí lo conoce")
  )

  # z: 0 = control (filtro NO administrado), 1 = tratamiento
  expect_equal(bd$drmnar_z, c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1))
  # tema elegido en el filtro (NA para control)
  expect_equal(bd$drmnar_tema[5], "Política")
  expect_true(all(is.na(bd$drmnar_tema[1:4])))
  # r por protocolo: control y tratamiento-Política responden; los
  # desertores del filtro son no respuesta del módulo político
  expect_equal(bd$drmnar_r, c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0))
  # dicotómica reservada para cálculos por pregunta: enmascarada si r = 0
  expect_equal(
    bd$drmnar_y_conoce_cruz,
    c(1, 0, 1, 1, 0, 1, NA, NA, NA, NA)
  )
  # la columna original NO se toca (en Chihuahua guarda la verdad de los
  # desertores, útil para validación)
  expect_equal(bd$conoce_cruz, snapshot_fixture()$conoce_cruz)
})

test_that("preparar_variables_drmnar exige la columna del instrumento", {
  expect_error(
    preparar_variables_drmnar(
      snapshot_fixture(),
      instrumento_campo = "no_existe"
    ),
    regexp = "no_existe"
  )
})

test_that("resumen_flujo_norespuesta reproduce los conteos de la Figure 2", {
  bd <- preparar_variables_drmnar(
    snapshot_fixture(),
    instrumento_campo = "gustos_aleatorio_50"
  )
  flujo <- resumen_flujo_norespuesta(bd)

  expect_s3_class(flujo, "tbl_df")
  expect_true(all(c("brazo", "etapa", "n") %in% names(flujo)))

  total_control <- flujo$n[flujo$brazo == "Control" & flujo$etapa == "Contactos"]
  expect_equal(total_control, 4)
  total_trat <- flujo$n[flujo$brazo == "Tratamiento" & flujo$etapa == "Contactos"]
  expect_equal(total_trat, 6)
  resp_trat <- flujo$n[flujo$brazo == "Tratamiento" &
                         flujo$etapa == "Respondió módulo político"]
  expect_equal(resp_trat, 2)
  # el desglose de deserción por tema suma los 4 desertores
  desercion <- flujo[flujo$etapa == "Desertó del filtro", ]
  expect_equal(sum(desercion$n), 4)
})

test_that("tabla_descriptivos_drmnar produce el formato de la Table A16", {
  bd <- preparar_variables_drmnar(
    snapshot_fixture(),
    instrumento_campo = "gustos_aleatorio_50",
    preguntas = list("conoce_cruz" = "Sí lo conoce")
  )
  tabla <- tabla_descriptivos_drmnar(
    bd,
    subconjuntos = list(
      "Todos" = NULL,
      "Mujeres" = bd$sexo == "F"
    )
  )
  expect_s3_class(tabla, "tbl_df")
  expect_true(all(c("indicador", "subconjunto", "valor", "n") %in% names(tabla)))

  # Z promedio en Todos: 6 de 10 en tratamiento
  z_todos <- tabla[tabla$indicador == "Z" & tabla$subconjunto == "Todos", ]
  expect_equal(z_todos$valor, 0.6)
  # R | Z = 1: 2 de 6
  r_z1 <- tabla[tabla$indicador == "R | Z=1" & tabla$subconjunto == "Todos", ]
  expect_equal(r_z1$valor, 2 / 6)
  # media de la dicotómica entre respondientes del módulo
  y_media <- tabla[tabla$indicador == "conoce_cruz" &
                     tabla$subconjunto == "Todos", ]
  expect_equal(y_media$valor, 4 / 6)
  expect_equal(y_media$n, 6)
})

test_that("el método de Preproceso aplica la preparación al snapshot nuevo", {
  # PreprocesoPrueba (helper-preproceso.R) permite instanciar sin el
  # constructor completo; el método opera sobre nuevos_registros_snapshot
  # justo antes de actualizar_bd() en el flujo de AppAuditoria
  pre <- PreprocesoPrueba$new(pool = NULL)
  pre$nuevos_registros_snapshot <- snapshot_fixture()

  pre$preparar_variables_dicotomicas_drmnar(
    instrumento_campo = "gustos_aleatorio_50",
    preguntas = list("conoce_cruz" = "Sí lo conoce")
  )

  snap <- pre$nuevos_registros_snapshot
  expect_true(all(c("drmnar_z", "drmnar_tema", "drmnar_r",
                    "drmnar_y_conoce_cruz") %in% names(snap)))
  expect_equal(snap$drmnar_r, c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0))

  # sin snapshot nuevo procesado => error claro
  pre2 <- PreprocesoPrueba$new(pool = NULL)
  expect_error(
    pre2$preparar_variables_dicotomicas_drmnar(
      instrumento_campo = "gustos_aleatorio_50"
    ),
    regexp = "procesar_nuevas_entradas"
  )
})

test_that("generar_diseno_drmnar arma el bundle con diagnóstico y decisión", {
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 21)
  # simular el contrato del snapshot: el diseño ya trae drmnar_z y la
  # pregunta; agregar covariable seccional ficticia
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z"
  )

  expect_s3_class(bundle, "diseno_drmnar")
  expect_true(all(c(
    "diseno", "diagnostico", "decision", "covariables", "instrumento"
  ) %in% names(bundle)))
  expect_s3_class(bundle$diseno, "survey.design")
  expect_s3_class(bundle$diagnostico, "tbl_df")
  # con gamma = 2 la decisión es DR-MNAR
  expect_equal(
    bundle$decision$decision[bundle$decision$pregunta == "conoce_cand"],
    "DR-MNAR"
  )
  expect_output(print(bundle), regexp = "DR-MNAR")
})

test_that("generar_diseno_drmnar respeta el override manual", {
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 21)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x",
    instrumento = "drmnar_z",
    override = tibble::tibble(
      pregunta = "conoce_cand",
      decision = "Raking",
      motivo = "decisión del analista para esta ola"
    )
  )
  expect_equal(
    bundle$decision$decision[bundle$decision$pregunta == "conoce_cand"],
    "Raking"
  )
  # el diagnóstico original se conserva intacto para auditoría
  expect_true(any(bundle$diagnostico$no_ignorable))
  expect_equal(bundle$override$motivo[1], "decisión del analista para esta ola")
})
