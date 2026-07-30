#' Title
#'
#' @description
#' A short description...
#'
#' @param encuesta
#' @param dir
#'
#' @return
#'
#' @examples
Auditoria <- R6::R6Class("Auditoria",
                         public = list(
                           dir = NULL,
                           initialize = function(encuesta,
                                                 tipo_encuesta,
                                                 dir = "auditoria"){
                             if(!file.exists(dir)){
                               dir.create(dir)
                             }
                             dir.create(glue::glue("{dir}/data"))
                             readr::write_rds(encuesta$Resultados, glue::glue("{dir}/data/clase_pregunta.rda"))
                             # readr::write_rds(encuesta$muestra$muestra, glue::glue("{dir}/data/diseno.rda"))
                             # readr::write_rds(encuesta$shp_completo, glue::glue("{dir}/data/shp.rda"))
                             # readr::write_excel_csv(encuesta$respuestas$base, glue::glue("{dir}/data/bd.csv"))
                             sf_use_s2(T)
                             mapa_base <- encuesta$shp_completo$shp$MUNICIPIO %>%
                               # Corrige geometrias validas en el plano pero invalidas para s2
                               #  (vertices duplicados) que rompen la disolucion por estrato
                               sf::st_make_valid() %>%
                               left_join(encuesta$muestra$muestra$poblacion$marco_muestral %>% distinct(MUNICIPIO,strata_1)) %>%
                               group_by(strata_1) %>% summarise(n()) %>%
                               sf::st_buffer(dist = 0)
                             readr::write_rds(mapa_base, glue::glue("{dir}/data/mapa_base.rda"))

                             enc_shp <- encuesta$respuestas$base %>%
                               filter(!is.na(Longitude) | !is.na(Latitude)) %>%
                               sf::st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")
                             readr::write_rds(enc_shp, glue::glue("{dir}/data/enc_shp.rda"))
                             # Calcular la relacióin entre cluster y seccion
                             encuesta$muestra$muestra$poblacion$marco_muestral |>
                               distinct(SECCION, cluster_2) |>
                               readr::write_rds(glue::glue("{dir}/data/catalogo_seccion_cluster.rda"))

                             # readr::write_excel_csv(encuesta$respuestas$eliminadas, glue::glue("{dir}/data/eliminadas.csv"))
                             if(tipo_encuesta == "inegi"){
                               file.copy(overwrite = T,
                                         from = system.file("app_inegi/app.R", package = "encuestar",
                                                            mustWork = TRUE),
                                         to = dir

                               )
                             }

                             if(tipo_encuesta == "ine"){
                               file.copy(overwrite = T,
                                         from = system.file("app_ine/app.R", package = "encuestar",
                                                            mustWork = TRUE),
                                         to = dir

                               )
                             }

                             self$dir <- dir
                           },
                           run_app = function(){
                             shiny::shinyAppDir(
                               self$dir
                             )
                           },
                           subir_app = function(...){
                             rsconnect::deployApp(self$dir,...)
                           }
                         ))
