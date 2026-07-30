# ==============================================================================
# Tests para Preproceso$actualizar_bd()
#
# Contrato esperado:
#   Bloque 1 (atómico):    agregar_nuevos_registros(con)  +
#                          actualizar_clusters_corregidos(con)
#   Bloque 2 (idempotente, siempre corre): marcar_registros_eliminados()
#
# El bloque 2 corre incluso cuando el bloque 1 falla, de modo que las reglas
# de eliminación siempre se aplican sobre los registros existentes.
# ==============================================================================

test_that("actualizar_bd llama los métodos en orden: agregar, clusters, marcar", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  llamadas <- character(0)

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) llamadas <<- c(llamadas, "agregar"),
    fn_clusters = function(con) llamadas <<- c(llamadas, "clusters"),
    fn_marcar   = function()    llamadas <<- c(llamadas, "marcar")
  )

  p$actualizar_bd()

  expect_equal(llamadas, c("agregar", "clusters", "marcar"))
})

test_that("agregar y clusters reciben la misma conexión; marcar es independiente", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  conexiones <- list()

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) conexiones$agregar  <<- con,
    fn_clusters = function(con) conexiones$clusters <<- con,
    fn_marcar   = function()    conexiones$marcar   <<- "pool_directo"
  )

  p$actualizar_bd()

  # agregar y clusters comparten exactamente el mismo objeto conexión
  expect_identical(conexiones$agregar, conexiones$clusters)
  # marcar no recibe esa misma conexión transaccional
  expect_false(identical(conexiones$agregar, conexiones$marcar))
})

test_that("si clusters falla: INSERT revertido, pero marcar SÍ corre", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  marcar_corrio <- FALSE

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) {
      DBI::dbAppendTable(con, "snapshot_id_99",
                         data.frame(SbjNum = 1L, eliminada_auditoria = 0L,
                                    eliminada_regla = 0L))
    },
    fn_clusters = function(con) stop("fallo simulado en clusters"),
    fn_marcar   = function()    marcar_corrio <<- TRUE
  )

  expect_error(p$actualizar_bd(), "nuevos registros no pudieron insertarse")

  # El INSERT debe haberse revertido
  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)
  expect_equal(n, 0L, label = "INSERT revertido por rollback")

  # Pero marcar sí debe haber corrido
  expect_true(marcar_corrio, label = "marcar_registros_eliminados corrió igual")
})

test_that("si agregar falla: snapshot intacto, pero marcar SÍ corre", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  marcar_corrio <- FALSE

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) stop("fallo simulado en INSERT"),
    fn_clusters = function(con) {},
    fn_marcar   = function()    marcar_corrio <<- TRUE
  )

  expect_error(p$actualizar_bd(), "nuevos registros no pudieron insertarse")

  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)
  expect_equal(n, 0L, label = "Snapshot sin cambios")

  expect_true(marcar_corrio, label = "marcar_registros_eliminados corrió igual")
})

test_that("si todo tiene éxito: INSERT persiste y marcar corre", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  marcar_corrio <- FALSE

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) {
      DBI::dbAppendTable(con, "snapshot_id_99",
                         data.frame(SbjNum = 3L, eliminada_auditoria = 0L,
                                    eliminada_regla = 0L))
    },
    fn_clusters = function(con) {},
    fn_marcar   = function()    marcar_corrio <<- TRUE
  )

  expect_no_error(p$actualizar_bd())

  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)
  expect_equal(n, 1L, label = "Registro guardado")

  expect_true(marcar_corrio, label = "marcar_registros_eliminados corrió")
})
