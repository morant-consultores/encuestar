# Subclase de Preproceso para pruebas.
# Bypasses el constructor complejo y permite inyectar comportamiento en los
# tres métodos privados que actualizar_bd() orquesta.
PreprocesoPrueba <- R6::R6Class(
  classname = "PreprocesoPrueba",
  inherit = encuestar::Preproceso,
  private = list(
    fn_agregar  = NULL,
    fn_marcar   = NULL,
    fn_clusters = NULL,
    agregar_nuevos_registros = function(con) {
      if (!is.null(private$fn_agregar)) private$fn_agregar(con)
    },
    marcar_registros_eliminados = function() {
      if (!is.null(private$fn_marcar)) private$fn_marcar()
    },
    actualizar_clusters_corregidos = function(con) {
      if (!is.null(private$fn_clusters)) private$fn_clusters(con)
    }
  ),
  public = list(
    initialize = function(pool, opinometro_id = 99L,
                          fn_agregar  = NULL,
                          fn_marcar   = NULL,
                          fn_clusters = NULL) {
      self$pool                      <- pool
      self$opinometro_id             <- opinometro_id
      self$nuevos_registros_snapshot <- NULL
      self$nuevos_registros_cluster  <- NULL
      self$sbj_eliminadas_auditoria  <- numeric(0)
      self$sbj_eliminadas_regla      <- numeric(0)
      private$fn_agregar  <- fn_agregar
      private$fn_marcar   <- fn_marcar
      private$fn_clusters <- fn_clusters
    }
  )
)

# Crea un pool SQLite en archivo temporal con la tabla snapshot lista.
# Devuelve list(pool, path). El llamador cierra el pool y borra el archivo.
crear_pool_sqlite <- function(id = 99L) {
  tmp  <- tempfile(fileext = ".sqlite")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = tmp)

  con <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(con), add = TRUE)
  DBI::dbCreateTable(con, glue::glue("snapshot_id_{id}"),
                     data.frame(SbjNum              = integer(),
                                eliminada_auditoria = integer(),
                                eliminada_regla     = integer()))

  list(pool = pool, path = tmp)
}
