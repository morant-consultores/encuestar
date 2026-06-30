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

test_that("marcar_registros_eliminados() rechaza SbjNum no numéricos en marcas_eliminacion", {
  # PreprocesoPruebaMarcar: no-op en Block 1 (INSERT/clusters) pero deja
  # marcar_registros_eliminados() real, donde vive la validación.
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPruebaMarcar$new(pool)
  # Inyectar caracteres como IDs — debe fallar antes de llegar a la BD
  p$marcas_eliminacion <- tibble::tibble(
    SbjNum              = c("42", "'; DROP TABLE snapshot_id_99; --"),
    eliminada_auditoria = c(1L, 1L),
    eliminada_regla     = c(0L, 0L)
  )

  expect_error(
    p$actualizar_bd(),
    regexp = "marcas_eliminacion.*numérico",
    label  = "SbjNum como caracteres deben lanzar error"
  )
})

test_that("marcar_registros_eliminados() rechaza SbjNum con NA o Inf", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p_na <- PreprocesoPruebaMarcar$new(pool)
  p_na$marcas_eliminacion <- tibble::tibble(
    SbjNum              = c(1, NA_real_),
    eliminada_auditoria = c(0L, 0L),
    eliminada_regla     = c(1L, 1L)
  )

  expect_error(
    p_na$actualizar_bd(),
    regexp = "marcas_eliminacion.*NA",
    label  = "NA en SbjNum debe lanzar error"
  )

  p_inf <- PreprocesoPruebaMarcar$new(pool)
  p_inf$marcas_eliminacion <- tibble::tibble(
    SbjNum              = Inf,
    eliminada_auditoria = 1L,
    eliminada_regla     = 0L
  )

  expect_error(
    p_inf$actualizar_bd(),
    regexp = "marcas_eliminacion.*NA",
    label  = "Inf en SbjNum debe lanzar error"
  )
})

test_that("marcar_registros_eliminados() acepta marcas bien formadas", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPruebaMarcar$new(pool)
  p$marcas_eliminacion <- tibble::tibble(
    SbjNum              = c(1L, 2L, 3L, 10L, 20L),
    eliminada_auditoria = c(1L, 1L, 1L, 0L, 0L),
    eliminada_regla     = c(0L, 0L, 0L, 1L, 1L)
  )

  # No debe lanzar error de validación. Puede fallar en dbExecute si SbjNum
  # no existe en la tabla snapshot vacía, pero la validación ya pasó.
  err <- tryCatch(p$actualizar_bd(), error = function(e) e)

  if (inherits(err, "error")) {
    # El error debe ser de la BD (p. ej. SQLite no soporta UPDATE...FROM de
    # SQL Server), no de la validación. Se compara contra los mensajes de
    # validar_ids_sql()/validar_opinometro_id(); el nombre de la tabla
    # temporal también contiene "marcas_eliminacion", así que no sirve
    # como discriminante.
    expect_false(
      grepl("debe ser un vector numérico|contiene valores NA|opinometro_id debe ser", err$message),
      label = "El error no debe ser de validación si los IDs son válidos"
    )
  } else {
    expect_true(TRUE, label = "actualizar_bd() completó sin error")
  }
})
