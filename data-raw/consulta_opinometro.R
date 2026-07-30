pool <- pool::dbPool(
  drv = odbc::odbc(),
  Driver= 'ODBC Driver 17 for SQL Server',
  Database = "SVNET",
  Server = "tcp:morant.database.windows.net",
  UID = Sys.getenv("pool_uid"),
  PWD = Sys.getenv("pool_pwd"),
  Port = 1433,
  timeout = 120
)

obtener_respuesta <- function(pool, codigos, encuesta_id){
  query_claves <- paste0("REPLACE(JSON_VALUE(r.Resultado, '$.", codigos, "'), 'ñ', 'n') AS ", codigos, collapse = ", ")
  encuesta_id <- encuesta_id |> toString()

  query <- glue::glue("
  SELECT
    r.Id,
    r.EncuestaId,
    r.FechaInicio,
    r.FechaFin,
    r.FechaCreada,
    r.UbicacionAplicada,
    r.UsuarioNum,
    r.isComplete,
    ", query_claves, "
  FROM
    Registros r
  WHERE EncuestaId in ({encuesta_id})
")

  dplyr::tbl(pool, dplyr::sql(query)) |>
    janitor::clean_names()
}

# id de prueba 163

hola <- obtener_respuesta(pool = pool,
                          codigos = c("usuario_num"),
                          encuesta_id = 163) |>
  dplyr::collect()

DBI::dbListTables(conn = pool)


hola2 <- tbl(pool, "Registros") |>
  filter(EncuestaId == 163) |>
  collect()

hola2 %>%
  purrr::pmap_df(function(Id, fecha, Resultado, UsuarioNum, ...) {
    # browser()
    Resultado |>
      jsonlite::fromJSON() |>
      as_tibble()

  }) |>
  colnames()

usuarios_163 <-
tbl(pool, "Usuarios") |>
  semi_join(
    tbl(pool, "UsuariosEncuesta") |>
      filter(EncuestaId == !!163), join_by(Id == UsuarioId)
  ) |>
  collect()

hola |>
  left_join(usuarios_163, by = c("usuario_num" = "Num")) |>
  filter(ubicacion_aplicada == ",") |>
  select(usuario_num, Nombre:AMaterno)



dplyr::tbl(src = pool, "Registros") |>
  filter(EncuestaId == 163) |>
  head() |>
  pull(Resultado)
  colnames()


usuarios_tabla <-
DBI::dbReadTable(conn = pool,
                 name = "Usuario") |>
  collect()

usuarios_tabla


hola_formateada <-
  hola |>
  select(UsuarioNum)
  bind_rows(hola) |>
  bind_rows(hola) |>
  bind_rows(hola) |>
  bind_rows(hola) |>
  tidyr::separate(col = ubicacion_aplicada,
                  into = c("Latitude", "Longitude"),
                  sep = ",",
                  remove = TRUE) %>%
  mutate(across(.cols = c(Latitude, Longitude),
                .fns = ~ na_if(., "")),
         sexo = sample(c("Hombre", "Mujer"),
                       size = nrow(.),
                       replace = TRUE),
         edad = sample(seq.int(from = 18, to = 99, by = 1),
                       size = nrow(.),
                       replace = TRUE)) |>
  select(Latitude, Longitude,
         sexo, edad)


bd_respuestas_hermosillo_agosto |>
  head(138) |>
  select(!c(SbjNum, Latitude, Longitude, edad, sexo)) |>
  bind_cols(hola_formateada)
