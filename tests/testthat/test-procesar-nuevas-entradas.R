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
