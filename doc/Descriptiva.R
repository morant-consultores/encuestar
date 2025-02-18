## ----setup, echo = FALSE------------------------------------------------------
library(encuestar)

## ----barras categorica, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$barras_categorica(codigo = 'voto_pm_24',
                                                       salto = 20,
                                                       porcentajes_fuera = T,
                                                       desplazar_porcentajes = 0.01)


## ----lollipops categorica, echo = TRUE, warning=FALSE, fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$lollipops_categorica(codigo = 'problema_principal',
                                                          limite_graf = 0.4,
                                                          width_cats = 25,
                                                          size = 3,
                                                          size_pct = 5)


## ----barras aspectos, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$barras_aspectos(patron_inicial = 'conoce_pm',
                                                     aspectos = c('astiazaran','delrio'), 
                                                     salto = 20,
                                                     porcentajes_fuera = T,
                                                     desplazar_porcentajes = 0.02)


## ----barras multirrespuesta, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$barras_multirespuesta(patron_inicial = 'medios_com_O',
                                                           salto = 20,
                                                           porcentajes_fuera = T,
                                                           desplazar_porcentajes = 0.02)


## ----lollipops_multirrespuesta, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$lollipops_multirespuesta(patron_inicial = 'medios_com_O', 
                                                              limite_graf = 0.7,
                                                              width_cats = 25,
                                                              size = 3,
                                                              size_pct = 5)



## ----gauge numerica, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$gauge_numerica(codigo = 'si_voto_24',
                                                    color = 'green',
                                                    escala = c(0,10),
                                                    size_text_pct = 25)


## ----gauge categorica, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$gauge_categorica(codigo = 'participacion_pm_21',
                                                      filtro = "respuesta == 'Sí'",
                                                      color = 'green',
                                                      escala = c(0,1),
                                                      size_text_pct = 25
                                                      )


## ----intervalo numerica, echo = TRUE, warning=FALSE,fig.width = 10, fig.height = 10----

encuesta_demo$Resultados$Descriptiva$intervalo_numerica(patron =  'afirmacion', 
                                                        aspectos = c('seguridad','economia','pais','hermosillo'), 
                                                        escala = c(0, 5), 
                                                        point_size = 1, 
                                                        text_point_size = 8)


