test_that("actualizar_bd llama los tres métodos privados en el orden correcto", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  llamadas <- character(0)

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) llamadas <<- c(llamadas, "agregar"),
    fn_marcar   = function(con) llamadas <<- c(llamadas, "marcar"),
    fn_clusters = function(con) llamadas <<- c(llamadas, "clusters")
  )

  p$actualizar_bd()

  expect_equal(llamadas, c("agregar", "marcar", "clusters"))
})

test_that("actualizar_bd pasa la misma conexión a los tres métodos privados", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  conexiones <- list()

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar  = function(con) conexiones$agregar  <<- con,
    fn_marcar   = function(con) conexiones$marcar   <<- con,
    fn_clusters = function(con) conexiones$clusters <<- con
  )

  p$actualizar_bd()

  expect_identical(conexiones$agregar,  conexiones$marcar)
  expect_identical(conexiones$agregar,  conexiones$clusters)
})

test_that("actualizar_bd hace rollback del INSERT cuando el paso 2 falla", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar = function(con) {
      DBI::dbAppendTable(con, "snapshot_id_99",
                         data.frame(SbjNum = 1L, eliminada_auditoria = 0L,
                                    eliminada_regla = 0L))
    },
    fn_marcar   = function(con) stop("fallo simulado en paso 2"),
    fn_clusters = function(con) {}
  )

  expect_error(p$actualizar_bd(), "fallo simulado en paso 2")

  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)

  expect_equal(n, 0L, label = "El INSERT debe haberse revertido por el rollback")
})

test_that("actualizar_bd hace rollback del INSERT cuando el paso 3 falla", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar = function(con) {
      DBI::dbAppendTable(con, "snapshot_id_99",
                         data.frame(SbjNum = 2L, eliminada_auditoria = 0L,
                                    eliminada_regla = 0L))
    },
    fn_marcar   = function(con) {},
    fn_clusters = function(con) stop("fallo simulado en paso 3")
  )

  expect_error(p$actualizar_bd(), "fallo simulado en paso 3")

  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)

  expect_equal(n, 0L, label = "El INSERT debe haberse revertido por el rollback")
})

test_that("actualizar_bd persiste los cambios cuando todo tiene éxito", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  p <- PreprocesoPrueba$new(
    pool,
    fn_agregar = function(con) {
      DBI::dbAppendTable(con, "snapshot_id_99",
                         data.frame(SbjNum = 3L, eliminada_auditoria = 0L,
                                    eliminada_regla = 0L))
    },
    fn_marcar   = function(con) {},
    fn_clusters = function(con) {}
  )

  expect_no_error(p$actualizar_bd())

  con <- pool::poolCheckout(pool)
  n   <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM 'snapshot_id_99'")$n
  pool::poolReturn(con)

  expect_equal(n, 1L, label = "El registro debe haberse guardado")
})
