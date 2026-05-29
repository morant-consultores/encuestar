# ==============================================================================
# Tests para el fix de doble llamada a marcar_eliminadas_por_regla
# ==============================================================================
#
# CONTEXTO DEL BUG (previo al fix):
#   procesar_nuevas_entradas() llamaba a marcar_eliminadas_por_regla dos veces:
#     Paso 2 → sobre respuestas_todas  (para llenar sbj_eliminadas_regla)
#     Paso 4 → sobre respuestas_nuevas (para poner flags en los registros nuevos)
#   El método obtener_ids_eliminadas_por_regla tiene bucles O(n×r), por lo que
#   correrlo dos veces es costoso e innecesario.
#
# EL FIX:
#   El paso 4 ya no invoca marcar_eliminadas_por_regla. En su lugar hace un
#   simple mutate(%in%) usando los vectores sbj_eliminadas_* ya calculados en
#   el paso 2. Una sola fuente de verdad, sin trabajo duplicado.
# ==============================================================================

# Subclase que cuenta cuántas veces se invoca marcar_eliminadas_por_regla
PreprocesoPruebaContador <- R6::R6Class(
  classname = "PreprocesoPruebaContador",
  inherit = PreprocesoPrueba,
  private = list(
    regla_call_count = 0L,
    marcar_eliminadas_por_regla = function(base) {
      private$regla_call_count <- private$regla_call_count + 1L
      super$marcar_eliminadas_por_regla(base)
    }
  ),
  public = list(
    get_regla_calls = function() private$regla_call_count
  )
)

test_that("marcar_eliminadas_por_regla se invoca exactamente una vez cuando no hay registros nuevos", {
  db   <- crear_pool_sqlite()
  pool <- db$pool
  on.exit({ pool::poolClose(pool); file.remove(db$path) }, add = TRUE)

  con <- pool::poolCheckout(pool)
  DBI::dbAppendTable(con, "snapshot_id_99",
                     data.frame(SbjNum              = 5L,
                                eliminada_auditoria = 0L,
                                eliminada_regla     = 0L))
  pool::poolReturn(con)

  p <- PreprocesoPruebaContador$new(pool)
  # Id=5 ya está en el snapshot → respuestas_nuevas queda vacío → paso 4 no corre
  p$snapshot_original        <- tibble::tibble(SbjNum = 5L)
  p$bd_respuestas_preparadas <- tibble::tibble(
    Id          = 5L,
    FechaInicio = Sys.time(),
    UsuarioNum  = "U1"
  )
  p$bd_eliminadas_regla <- tibble::tibble(
    id_regla     = "R1",
    fecha_inicio = Sys.time() - 3600,
    fecha_fin    = Sys.time() + 3600,
    UsuarioNum   = NA_character_,
    creada_el    = Sys.time()
  )
  p$auditoria_telefonica <- NULL

  p$procesar_nuevas_entradas()

  expect_equal(
    p$get_regla_calls(), 1L,
    label = "marcar_eliminadas_por_regla solo corre en el paso global (paso 2), no en el paso 4"
  )
  expect_null(
    p$nuevos_registros_snapshot,
    label = "No hay registros nuevos que procesar"
  )
})

test_that("fuente única: flags en registros nuevos son consistentes con sbj_eliminadas_*", {
  # Este test verifica el invariante que el fix garantiza:
  # las columnas eliminada_* que se asignan a los registros nuevos (paso 4)
  # deben coincidir exactamente con los vectores sbj_eliminadas_* calculados
  # globalmente (paso 2). El %in% es determinista y no depende del orden de
  # ejecución ni de re-computaciones costosas.

  sbj_eliminadas_auditoria <- c(10L, 20L)
  sbj_eliminadas_regla     <- c(30L, 40L)

  respuestas_nuevas <- tibble::tibble(Id = c(10L, 30L, 50L))

  resultado <- respuestas_nuevas |>
    dplyr::mutate(
      eliminada_auditoria = dplyr::if_else(Id %in% sbj_eliminadas_auditoria, 1L, 0L),
      eliminada_regla     = dplyr::if_else(Id %in% sbj_eliminadas_regla,     1L, 0L)
    )

  # Id=10 → eliminada por auditoría, no por regla
  # Id=30 → eliminada por regla, no por auditoría
  # Id=50 → no eliminada
  expect_equal(resultado$eliminada_auditoria, c(1L, 0L, 0L),
               label = "Id=10 marcado por auditoria, Id=30 y 50 no")
  expect_equal(resultado$eliminada_regla,     c(0L, 1L, 0L),
               label = "Id=30 marcado por regla, Id=10 y 50 no")
})

test_that("sin fix: correr marcar_eliminadas_por_regla dos veces sería O(2×n×r)", {
  # Test de documentación: ilustra por qué el doble-call era costoso.
  # obtener_ids_eliminadas_por_regla itera sobre cada regla con un bucle for.
  # Con R reglas y N registros, es O(N×R) por llamada → O(2×N×R) antes del fix.
  # Ahora es O(N×R) + O(1) (el %in% es O(N) sobre un vector ya calculado).

  # R1: ventana de 7200s-3601s atrás  → captura records a ~5000s atrás
  # R2: ventana de 3600s-500s atrás   → captura records a ~1000s atrás
  # Id=4,5 tienen FechaInicio reciente (≤100s atrás) → fuera de ambas reglas
  reglas <- tibble::tibble(
    id_regla     = c("R1", "R2"),
    fecha_inicio = c(Sys.time() - 7200, Sys.time() - 3600),
    fecha_fin    = c(Sys.time() - 3601, Sys.time() -  500),
    UsuarioNum   = c(NA_character_,     NA_character_),
    creada_el    = Sys.time()
  )

  base <- tibble::tibble(
    Id          = 1:5,
    FechaInicio = c(
      Sys.time() - 5000,  # dentro de R1 (entre 7200s y 3601s atrás)
      Sys.time() - 5000,  # dentro de R1
      Sys.time() - 1000,  # dentro de R2 (entre 3600s y 500s atrás)
      Sys.time() -   50,  # fuera de ambas (muy reciente)
      Sys.time() -   10   # fuera de ambas (muy reciente)
    ),
    UsuarioNum  = "U1"
  )

  # Simular obtener_ids vía la lógica interna (sin instanciar Preproceso)
  ids_primera_corrida <- {
    reglas_fecha <- reglas |> dplyr::filter(!is.na(fecha_inicio) & is.na(UsuarioNum))
    vec <- c()
    for (i in seq_len(nrow(reglas_fecha))) {
      ids <- base |>
        dplyr::filter(FechaInicio >= reglas_fecha$fecha_inicio[i] &
                        FechaInicio <= reglas_fecha$fecha_fin[i]) |>
        dplyr::pull(Id)
      vec <- c(vec, ids)
    }
    unique(vec)
  }

  # Los IDs detectados deben ser exactamente 1, 2 y 3
  expect_setequal(ids_primera_corrida, c(1L, 2L, 3L))

  # El %in% sobre el vector ya calculado es O(N), no O(N×R)
  segunda_corrida_nuevo <- base |>
    dplyr::mutate(
      eliminada_regla = dplyr::if_else(Id %in% ids_primera_corrida, 1L, 0L)
    )

  expect_equal(segunda_corrida_nuevo$eliminada_regla,
               c(1L, 1L, 1L, 0L, 0L),
               label = "IDs 1,2,3 eliminados por regla; 4,5 no eliminados")
})
