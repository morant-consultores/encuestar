# ==============================================================================
# Tests para validar_opinometro_id() y validar_ids_sql()
# ==============================================================================
#
# CONTEXTO:
#   marcar_registros_eliminados() interpola directamente opinometro_id y los
#   vectores de IDs eliminados en queries SQL (FreeTDS no soporta placeholders
#   de forma confiable). Estas validaciones protegen contra IDs mal tipados que
#   generarían queries malformadas o nombres de tabla inválidos.
# ==============================================================================

test_that("actualizar_bd() rechaza opinometro_id no entero o no positivo", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  ids_invalidos <- list(
    NULL,           # ausente
    "99",           # cadena de texto
    99.5,           # decimal
    -1L,            # negativo
    0L,             # cero
    Inf,            # infinito
    NA_real_        # NA
  )

  for (id in ids_invalidos) {
    p <- PreprocesoPrueba$new(pool, opinometro_id = id)
    expect_error(
      p$actualizar_bd(),
      regexp = "opinometro_id",
      label  = paste("opinometro_id =", deparse(id), "debe lanzar error")
    )
  }
})

test_that("actualizar_bd() acepta opinometro_id entero positivo", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  ids_validos <- list(1L, 99L, 100L, 1L)

  for (id in ids_validos) {
    p <- PreprocesoPrueba$new(pool, opinometro_id = id)
    # Con las inyecciones de no-ops, actualizar_bd() debe completar sin error
    expect_no_error(
      p$actualizar_bd(),
      message = paste("opinometro_id =", deparse(id), "no debe lanzar error")
    )
  }
})

test_that("marcar_registros_eliminados() rechaza IDs no numéricos en sbj_eliminadas_*", {
  # PreprocesoPruebaMarcar: no-op en Block 1 (INSERT/clusters) pero deja
  # marcar_registros_eliminados() real, donde vive la validación.
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPruebaMarcar$new(pool)
  # Inyectar caracteres como IDs — debe fallar antes de llegar a dbExecute
  p$sbj_eliminadas_auditoria <- c("42", "'; DROP TABLE snapshot_id_99; --")
  p$sbj_eliminadas_regla     <- numeric(0)

  expect_error(
    p$actualizar_bd(),
    regexp = "sbj_eliminadas_auditoria.*numérico",
    label  = "IDs de auditoría como caracteres deben lanzar error"
  )
})

test_that("marcar_registros_eliminados() rechaza IDs con NA o Inf", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p_na <- PreprocesoPruebaMarcar$new(pool)
  p_na$sbj_eliminadas_auditoria <- numeric(0)
  p_na$sbj_eliminadas_regla     <- c(1L, NA_real_)

  expect_error(
    p_na$actualizar_bd(),
    regexp = "sbj_eliminadas_regla.*NA",
    label  = "NA en IDs de regla debe lanzar error"
  )

  p_inf <- PreprocesoPruebaMarcar$new(pool)
  p_inf$sbj_eliminadas_auditoria <- c(Inf)
  p_inf$sbj_eliminadas_regla     <- numeric(0)

  expect_error(
    p_inf$actualizar_bd(),
    regexp = "sbj_eliminadas_auditoria.*NA",
    label  = "Inf en IDs de auditoría debe lanzar error"
  )
})

test_that("marcar_registros_eliminados() acepta vectores numéricos bien formados", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPruebaMarcar$new(pool)
  p$sbj_eliminadas_auditoria <- c(1L, 2L, 3L)
  p$sbj_eliminadas_regla     <- c(10L, 20L)

  # No debe lanzar error de validación. Puede fallar en dbExecute si SbjNum
  # no existe en la tabla snapshot vacía, pero la validación ya pasó.
  err <- tryCatch(p$actualizar_bd(), error = function(e) e)

  if (inherits(err, "error")) {
    # El error debe ser de la BD, no de la validación
    expect_false(
      grepl("sbj_eliminadas|opinometro_id", err$message),
      label = "El error no debe ser de validación si los IDs son válidos"
    )
  } else {
    expect_true(TRUE, label = "actualizar_bd() completó sin error")
  }
})
