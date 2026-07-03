# Prepara las tablas delgadas de covariables seccionales para DR-MNAR ----
#
# Este script NO se ejecuta al instalar el paquete: deriva tablas ligeras
# (una fila por sección, ~6 columnas) desde los insumos pesados que viven
# FUERA del repositorio (el censo INEGI por sección pesa ~119 MB y no se
# versiona), y las guarda en inst/extdata/ para usarlas como fixture y
# como insumo directo en proyectos.
#
# Insumos esperados (ajusta las rutas si tu carpeta difiere):
#   - Censo 2020 por sección (INEGI):
#       ../doubly robust estimator/inegi/seccion_2020.rda   [saveRDS]
#   - Histórico electoral por entidad (objeto Tablero de aelectoral):
#       ../Encuestas-edomex-2025/Enero/data/mex.rda         [saveRDS]
#
# Uso: source("data-raw/preparar_covariables_seccion.R") desde la raíz
# del paquete encuestar.

devtools::load_all(".")

raiz <- normalizePath("..")
path_censo <- file.path(raiz, "doubly robust estimator", "inegi", "seccion_2020.rda")
path_electoral_mex <- file.path(raiz, "Encuestas-edomex-2025", "Enero", "data", "mex.rda")

stopifnot(file.exists(path_censo))
# pese a la extensión .rda, ambos archivos se guardaron con saveRDS()
censo <- readRDS(path_censo)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

# ---- Estado de México: censo + histórico electoral (ejemplo completo) ----
if (file.exists(path_electoral_mex)) {
  tablero_mex <- readRDS(path_electoral_mex)
  electoral_mex <- tablero_mex$info$bd
  # tres últimos procesos concurrentes con cobertura estatal (Sección 4
  # del Anexo Técnico: 2018, 2021 y 2024)
  covariables_seccion_mex <- construir_covariables_seccion(
    censo_seccion = dplyr::filter(censo, entidad == "15"),
    electoral_bd = electoral_mex,
    elecciones = c("pr_24", "gb_23", "dl_21", "pr_18")
  )
  saveRDS(
    covariables_seccion_mex,
    "inst/extdata/covariables_seccion_mex.rds",
    compress = "xz"
  )
  message(
    "covariables_seccion_mex.rds: ", nrow(covariables_seccion_mex),
    " secciones."
  )
} else {
  message("mex.rda no disponible; se omite el derivado de Edomex.")
}

# ---- Chihuahua: solo censo (el histórico electoral seccional de la ----
# entidad aún no está disponible en el repositorio; cuando el equipo lo
# integre como objeto aelectoral, agregar electoral_bd + elecciones igual
# que en el bloque de Edomex)
covariables_seccion_chih <- construir_covariables_seccion(
  censo_seccion = dplyr::filter(censo, entidad == "08")
)
saveRDS(
  covariables_seccion_chih,
  "inst/extdata/covariables_seccion_chih.rds",
  compress = "xz"
)
message(
  "covariables_seccion_chih.rds: ", nrow(covariables_seccion_chih),
  " secciones."
)
