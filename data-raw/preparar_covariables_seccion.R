# Referencia: derivar covariables seccionales para DR-MNAR (POR PROYECTO)
# ---------------------------------------------------------------------
#
# IMPORTANTE (arquitectura): `encuestar` y `muestreaR` son paqueterías
# GENERALES. El censo INEGI y el histórico electoral son INSUMOS DE
# PROYECTO, no del paquete: cambian por entidad y por ola. Por eso el
# derivado `covariables_seccion.rds` se guarda en el REPO DE LA OLA
# (p. ej. `encuesta/insumos/`), NO dentro del paquete.
#
# El paquete solo aporta la FUNCIÓN construir_covariables_seccion(); este
# archivo es una plantilla de referencia para copiar al proyecto.
#
# Insumos (rutas relativas al workspace `encuestas-morant/`):
#   - Censo 2020 por sección (NACIONAL, se filtra por entidad):
#       doubly robust estimator/inegi/seccion_2020.rda      (saveRDS)
#   - Histórico electoral de la entidad (objeto Tablero de aelectoral,
#     vive en el repo de alguna ola de esa entidad), p. ej.:
#       enc_chihuahua_nov2024/Insumos/chih.rda
#
# El índice de marginación NO se baja de CONAPO: se DERIVA de los
# indicadores del censo con la fórmula estándar; el objeto aelectoral ya
# expone el rezago social por sección vía su método calcular_irs().

library(encuestar)

# --- ajusta estas 3 rutas a tu proyecto ---
entidad <- "08" # Chihuahua
path_censo <- "../doubly robust estimator/inegi/seccion_2020.rda"
path_electoral <- "../enc_chihuahua_nov2024/Insumos/chih.rda"
salida <- "insumos/covariables_seccion.rds" # DENTRO del repo de la ola
# ------------------------------------------

censo <- readRDS(path_censo) # pese a la extensión .rda es saveRDS
tablero <- readRDS(path_electoral) # objeto aelectoral (R6 Tablero)
electoral_bd <- tablero$info$bd # base electoral por sección

covariables_seccion <- construir_covariables_seccion(
  censo_seccion = dplyr::filter(censo, entidad == !!entidad),
  electoral_bd = electoral_bd,
  # elecciones con cobertura estatal de dos ciclos
  elecciones = c("pr_24", "dl_24", "gb_21", "dl_21")
)

dir.create(dirname(salida), showWarnings = FALSE, recursive = TRUE)
saveRDS(covariables_seccion, salida, compress = "xz")
message(nrow(covariables_seccion), " secciones -> ", salida)
