# ==============================================================================
# Tests para Preproceso$procesar_nuevas_entradas()
# ==============================================================================

test_that("auto-colecta snapshot lazy y el anti_join filtra correctamente", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  # Registro preexistente en el snapshot
  con <- pool::poolCheckout(pool)
  DBI::dbAppendTable(con, "snapshot_id_99",
                     data.frame(SbjNum              = 42L,
                                eliminada_auditoria = 0L,
                                eliminada_regla     = 0L))
  pool::poolReturn(con)

  p <- PreprocesoPrueba$new(pool)

  # Asignar tbl lazy como snapshot_original (el caso problemático)
  p$snapshot_original    <- dplyr::tbl(pool, "snapshot_id_99")
  p$bd_respuestas_preparadas <- tibble::tibble(Id = 42L)

  expect_true(inherits(p$snapshot_original, "tbl_lazy"),
              label = "Antes: snapshot_original es lazy")

  p$procesar_nuevas_entradas()

  # El guard debe haber colectado el snapshot
  expect_false(inherits(p$snapshot_original, "tbl_lazy"),
               label = "Después: snapshot_original fue colectado")
  expect_s3_class(p$snapshot_original, "data.frame")

  # El anti_join corrió correctamente: Id=42 ya estaba en el snapshot,
  # por lo que no hay nuevos registros que procesar
  expect_null(p$nuevos_registros_snapshot,
              label = "Ningún registro nuevo porque 42 ya existe en el snapshot")
})

test_that("sin guard: nrow(lazy) devuelve NA y el anti_join no corre", {
  # Este test documenta el comportamiento ROTO (sin el guard) para que quede
  # claro qué problema resuelve el fix.
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  con <- pool::poolCheckout(pool)
  DBI::dbAppendTable(con, "snapshot_id_99",
                     data.frame(SbjNum              = 42L,
                                eliminada_auditoria = 0L,
                                eliminada_regla     = 0L))
  pool::poolReturn(con)

  lazy_snap <- dplyr::tbl(pool, "snapshot_id_99")

  # nrow() sobre un tbl lazy devuelve NA, no el número real de filas
  expect_true(is.na(nrow(lazy_snap)),
              label = "nrow(tbl_lazy) es NA, no 1")

  # NA > 0 es NA — el if lo evalúa como falso y el anti_join nunca corre
  expect_false(isTRUE(nrow(lazy_snap) > 0),
               label = "NA > 0 no es TRUE, el if queda en falso")
})

test_that("marcas_eliminacion lleva también los 0: borrar una regla restaura registros", {
  # La sincronización en actualizar_bd() escribe el estado completo (0 y 1)
  # de cada registro evaluado. Esto garantiza que un registro marcado como
  # eliminada_regla = 1 en una corrida anterior se restaure cuando la regla
  # que lo eliminaba ya no existe.
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  # Id=5 quedó marcado por una regla que después fue borrada
  con <- pool::poolCheckout(pool)
  DBI::dbAppendTable(con, "snapshot_id_99",
                     data.frame(SbjNum              = 5L,
                                eliminada_auditoria = 0L,
                                eliminada_regla     = 1L))
  pool::poolReturn(con)

  p <- PreprocesoPrueba$new(pool)
  p$snapshot_original        <- tibble::tibble(SbjNum = 5L)
  p$bd_respuestas_preparadas <- tibble::tibble(
    Id          = 5L,
    FechaInicio = Sys.time(),
    UsuarioNum  = "U1"
  )
  p$bd_eliminadas_regla  <- NULL # la regla ya no existe
  p$auditoria_telefonica <- NULL

  p$procesar_nuevas_entradas()

  expect_equal(p$marcas_eliminacion$SbjNum, 5)
  expect_equal(
    p$marcas_eliminacion$eliminada_regla, 0L,
    label = "Sin regla vigente, la marca evaluada es 0 (señal de restauración)"
  )
  expect_equal(p$marcas_eliminacion$eliminada_auditoria, 0L)
})

test_that("marcas_eliminacion refleja 1 para registros que caen en una regla vigente", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPrueba$new(pool)
  p$snapshot_original        <- tibble::tibble(SbjNum = c(5L, 6L))
  p$bd_respuestas_preparadas <- tibble::tibble(
    Id          = c(5L, 6L),
    FechaInicio = c(Sys.time(), Sys.time() - 10000),
    UsuarioNum  = c("U1", "U2")
  )
  # Regla vigente que captura sólo a Id=5 (por usuario)
  p$bd_eliminadas_regla <- tibble::tibble(
    id_regla     = "R1",
    fecha_inicio = as.POSIXct(NA),
    fecha_fin    = as.POSIXct(NA),
    UsuarioNum   = "U1",
    creada_el    = Sys.time()
  )
  p$auditoria_telefonica <- NULL

  p$procesar_nuevas_entradas()

  marcas <- p$marcas_eliminacion |> dplyr::arrange(SbjNum)
  expect_equal(marcas$SbjNum, c(5, 6))
  expect_equal(marcas$eliminada_regla, c(1L, 0L),
               label = "Id=5 marcado por la regla de usuario, Id=6 limpio")
})
