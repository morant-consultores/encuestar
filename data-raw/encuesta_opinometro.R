# SCRIPT DE PRUEBAS PARA GENERAR UNA CLASE ENCUESTA USANDO COMO INSUMO EL OPINOMETRO
# Al generar la clase, se escriben los archivos R/constantes.R, R/funciones.R y un folder
# llamado auditoria/ los cuales no forman parte edl desarrollo salvo que se esté trabajando en la
# encuesta demo

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

pool = pool::dbPool(
    drv = odbc::odbc(),
    Driver= 'ODBC Driver 17 for SQL Server',
    Database = "SVNET",
    Server = "tcp:morant.database.windows.net",
    UID = Sys.getenv("pool_uid"),
    PWD = Sys.getenv("pool_pwd"),
    Port = 1433,
    timeout = 120)

shp_hermosillo_agosto <-
  readr::read_rds("./data-raw/shp.rda")

diseno_hermosillo_agosto <-
  readr::read_rds("./data-raw/diseno.rda")

diccionario_sonora_opinometro <-
  readxl::read_xlsx(path = "./data-raw/diccionario_sonora_20240802.xlsx") |>
  mutate(llaves = gsub(pattern = "prioridad_gobireno",
                       replacement = "prioridad_gobierno",
                       x = llaves))

# Base de eliminadas --------------------------------------------------------------------------

eliminadas <-
  tibble(SbjNum = 000,
         razon = sample(c("razon_a", "razon_b", "razon_c"),
                        size = 1,
                        replace = TRUE))

quitar <- c()

mantener <- ""

# Clase -------------------------------------------------------------------

encuesta_opinometro <- Encuesta$new(respuestas = NA,
                                    # n_simulaciones = 200,
                                    opinometro_id = 167,
                                    pool = pool,
                                    quitar_vars = quitar,
                                    mantener = mantener,
                                    bd_correcciones = NULL,
                                    muestra = diseno_hermosillo_agosto,
                                    auditoria_telefonica = eliminadas,
                                    cuestionario = diccionario_sonora_opinometro,
                                    shp = shp_hermosillo_agosto,
                                    sin_peso = T,
                                    mantener_falta_coordenadas = F, # mantener entrevistas sin coordenadas
                                    rake = T, ######### con postestratificacion
                                    patron = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
                                    auditar = c("")
)

options(survey.lonely.psu = "remove")

# encuesta_opinometro$auditoria$run_app()
