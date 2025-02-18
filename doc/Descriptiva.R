## ----setup,message=FALSE, echo = FALSE----------------------------------------
library(encuestar)
library(dplyr)
library(ggplot2)

## ----barras categorica, echo = TRUE, warning=FALSE,fig.width = 14, fig.height = 7.56 ,eval=FALSE----
#  
#  encuesta_demo$Resultados$Descriptiva$barras_categorica(codigo = 'voto_pm_24',
#                                                         salto = 30,
#                                                         porcentajes_fuera = T,
#                                                         desplazar_porcentajes = 0.01)
#  

## ----barras categorica graf, echo = FALSE, warning=FALSE, eval=TRUE, fig.width = 14, fig.height = 7.56----

desc_1 <- encuesta_demo$Resultados$Descriptiva$barras_categorica(codigo = 'voto_pm_24',
                                                       salto = 30,
                                                       porcentajes_fuera = T,
                                                       desplazar_porcentajes = 0.01)+
  theme(text = element_text(family = "Poppins"), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 18))

desc_1

## ----barras categorica muestra, echo = TRUE, eval=FALSE-----------------------
#  encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = 'voto_pm_24') |>
#    mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = 0.01, w = media, other_level = "Otros"),
#           respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
#    group_by(respuesta) |>
#    summarise(media = sum(media)) |>
#    encuestar:::graficar_barras(salto = 30,
#                                porcentajes_fuera = T,
#                                desplazar_porcentajes = 0.01)+
#    encuestar::tema_morant()

## ----lollipops categorica, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$lollipops_categorica(codigo = 'problema_principal',
                                                          limits = c(0,0.4),
                                                          width_cats = 25,
                                                          size = 3,
                                                          size_pct = 5)


## ----lollipops categorica muestra, echo = TRUE, eval=FALSE--------------------
#  encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = 'problema_principal') |>
#    mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = 0.01, w = media, other_level = "Otros"),
#           respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
#    group_by(respuesta) |>
#    summarise(media = sum(media)) |>
#    rename(pct = media) |>
#    encuestar:::graficar_lollipops(limits = c(0,0.4),
#                                   width_cats = 25 ,
#                                   size = 3,
#                                   size_pct = 5)+
#    encuestar::tema_morant()

## ----dicc_ejemp_1,warning=FALSE,message=FALSE,echo = FALSE--------------------

ejemp_dic_1 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('conoce_pm',llaves))



knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm" ', align = 'c')


## ----barras aspectos, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$barras_aspectos(patron_inicial = 'conoce_pm',
                                                     aspectos = c('astiazaran','delrio'), 
                                                     salto = 20,
                                                     filtro = "respuesta == 'Sí'",
                                                     porcentajes_fuera = T,
                                                     desplazar_porcentajes = 0.02)


## ----barras aspectos muestra, echo = TRUE, eval=FALSE-------------------------
#  encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno,
#                                            diccionario = encuesta_demo$cuestionario$diccionario,
#                                            patron_pregunta = 'conoce_pm',
#                                            aspectos =  c('astiazaran','delrio')) |>
#    left_join(encuesta_demo$cuestionario$diccionario|>
#                select(llaves, tema), by = c("aspecto" = "llaves")) |>
#    filter(respuesta == 'Sì') |>
#    transmute(respuesta = tema, media) |>
#    encuestar:::graficar_barras(salto = 20,
#                                porcentajes_fuera = T,
#                                desplazar_porcentajes = 0.02,
#                                orden_respuestas = NA)+
#    encuestar::tema_morant()

## ----dicc_ejemp_2,warning=FALSE,message=FALSE,echo = FALSE--------------------

ejemp_dic_2 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,respuestas)|>
  filter(grepl('medios_com_O',llaves))



knitr::kable(ejemp_dic_2, caption = 'Tabla 2. Llaves con prefijo "medios_com_O" ', align = 'c')


## ----barras multirrespuesta, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$barras_multirespuesta(patron_inicial = 'medios_com_O',
                                                           salto = 20,
                                                           porcentajes_fuera = T,
                                                           desplazar_porcentajes = 0.02)


## ----barras multirrespuesta muestra, echo = TRUE, eval=FALSE------------------
#  encuestar:::analizar_frecuencia_multirespuesta(diseno = encuesta_demo$muestra$diseno,
#                                                 'medios_com_O') %>%
#    encuestar:::graficar_barras(salto = 20,
#                                porcentajes_fuera = T,
#                                desplazar_porcentajes = 0.02)+
#    encuestar::tema_morant()

## ----dicc_ejemp_2_1,warning=FALSE,message=FALSE,echo = FALSE------------------

knitr::kable(ejemp_dic_2, caption = 'Tabla 2. Llaves con prefijo "medios_com_O" ', align = 'c')


## ----lollipops_multirrespuesta, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$lollipops_multirespuesta(patron_inicial = 'medios_com_O', 
                                                              limits = c(0,0.7),
                                                              width_cats = 25,
                                                              size = 3,
                                                              size_pct = 5)



## ----lollipops multirrespuesta muestra, echo = TRUE, eval=FALSE---------------
#  encuestar:::analizar_frecuencia_multirespuesta(diseno = encuesta_demo$muestra$diseno,
#                                                 'medios_com_O') %>%
#    rename(pct = media) |>
#    encuestar:::graficar_lollipops(limits = c(0,0.7),
#                                   width_cats = 25,
#                                   size = 3,
#                                   size_pct = 5)+
#    encuestar::tema_morant()

## ----gauge numerica, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$gauge_numerica(codigo = 'si_voto_24',
                                                    color = 'green',
                                                    escala = c(0,10),
                                                    size_text_pct = 25)


## ----auge numerica muestra, echo = TRUE, eval=FALSE---------------------------
#  encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = 'si_voto_24')|>
#    encuestar:::graficar_gauge(color_principal = 'green',color_secundario = "gray80",
#                               escala = c(0,10),
#                               size_text_pct = 25)

## ----gauge categorica, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$gauge_categorica(codigo = 'participacion_pm_21',
                                                      filtro = "respuesta == 'Sí'",
                                                      color = 'green',
                                                      escala = c(0,1),
                                                      size_text_pct = 25
                                                      )


## ----gauge categorica muestra, echo = TRUE, eval=FALSE------------------------
#  encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = 'participacion_pm_21')|>
#    filter(respuesta == 'Sí')|>
#    encuestar:::graficar_gauge(color_principal = 'green',color_secundario = "gray80",
#                               escala = c(0,1),
#                               size_text_pct = 25)
#  

## ----dicc_ejemp_3,warning=FALSE,message=FALSE,echo = FALSE--------------------

ejemp_dic_3 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,tema,llaves,respuestas)|>
  filter(grepl('afirmacion',llaves))



knitr::kable(ejemp_dic_3, caption = 'Tabla 3. Llaves con prefijo "afirmacion" ', align = 'c')


## ----intervalo numerica, echo = TRUE, warning=FALSE, fig.width = 14, fig.height = 7.56----

encuesta_demo$Resultados$Descriptiva$intervalo_numerica(patron =  'afirmacion', 
                                                        aspectos = c('seguridad','economia','pais','hermosillo'), 
                                                        escala = c(0, 5), 
                                                        point_size = 1, 
                                                        text_point_size = 8)


## ----intervalo numerica muestra, echo = TRUE, eval=FALSE----------------------
#  encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno,
#                                            diccionario = encuesta_demo$cuestionario$diccionario,
#                                            patron_pregunta = 'afirmacion',
#                                            aspectos = c('seguridad','economia','pais','hermosillo'))|>
#    left_join(encuesta_demo$cuestionario$diccionario %>%
#                select(aspecto = llaves, tema), by = "aspecto") |>
#    encuestar:::graficar_intervalo_numerica(escala = c(0, 5),
#                                            point_size = 1,
#                                            text_point_size = 8)+
#    encuestar::tema_morant()
#  

