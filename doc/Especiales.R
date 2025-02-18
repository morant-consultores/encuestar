## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE, 
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(encuestar)

## ----dicc_ejemp_1,warning=FALSE,message=FALSE,echo = FALSE--------------------
library(dplyr)
ejemp_dic_1 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('opinion_pm',llaves)|grepl('conoce_pm',llaves))|>
  filter(!grepl('razon|lia|javier',llaves))



knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm" y "opinion_pm"', align = 'c')


## ----ejemplo1, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Especial$candidatoOpinion(patron_inicial = "opinion_pm",
                                                   aspectos = c("astiazaran", "delrio"),
                                                   ns_nc = "Ns/Nc",
                                                   regular = "Regular",
                                                   llave_burbuja = "conoce_pm",
                                                   filtro_burbuja = "respuesta == 'Sí'",
                                                   grupo_positivo = c("Buena", "Muy buena"),
                                                   grupo_negativo = rev(c("Muy mala", "Mala")),
                                                   orden_resp = c("Muy mala", "Mala", "Regular", "Buena", "Muy buena"),
                                                   colores_opinion = c("Muy mala" = "#4A4E69",
                                                                       "Mala" = "#606299",
                                                                       "Regular" = "#D5B9B2",
                                                                       "Buena" = "#52B788",
                                                                       "Muy buena" = "#2D6A4F"),
                                                   color_nsnc = "gray60",
                                                   color_burbuja = "#A6032F")


## ----dicc_ejemp_2,warning=FALSE,message=FALSE,echo = FALSE--------------------

ejemp_dic_2 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('opinion_pm',llaves))|>
  filter(!grepl('razon|lia|javier',llaves))



knitr::kable(ejemp_dic_2, caption = 'Tabla 2. Llaves con prefijo "conoce_pm"', align = 'c')


## ----ejemplo2, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Especial$candidatoSaldo(llave_opinion = "opinion_pm", 
                                                 candidatos = c("astiazaran", "delrio"), 
                                                 positivos = c("Buena", "Muy buena"), 
                                                 negativos = rev(c("Muy mala", "Mala")), 
                                                 regular = "Regular", 
                                                 ns_nc = "Ns/Nc", 
                                                 color_positivo = "#52B788", 
                                                 color_negativo = "#606299", 
                                                 caption_opinion = "")


## ----dicc_ejemp_1_1,warning=FALSE,message=FALSE,echo = FALSE------------------

knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm" y "opinion_pm"', align = 'c')


## ----ejemplo3, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Especial$candidatoOpinion2(patron_opinion = "opinion_pm",
                                                    aspectos = c("astiazaran", "delrio"),
                                                    ns_nc = "Ns/Nc",
                                                    regular = "Regular",
                                                    patron_conocimiento = "conoce_pm",
                                                    filtro_conocimiento = "respuesta == 'Sí'",
                                                    orden_opinion = rev(c("Muy mala", "Mala", "Regular", "Buena", "Muy buena")),
                                                    colores_opinion = c("Muy mala" = "#4A4E69",
                                                                        "Mala" = "#606299",
                                                                        "Regular" = "#D5B9B2",
                                                                        "Buena" = "#52B788",
                                                                        "Muy buena" = "#2D6A4F",
                                                                        "Ns/Nc" = "gray70"),
                                                    colores_candidato = c("Antonio \"Toño\" Astiazarán" = "red",
                                                                          "María Dolores Del Río" = "blue"),
                                                    color_principal = "white",
                                                    color_conocimiento = "orange")

## ----dicc_ejemp_3,warning=FALSE,message=FALSE,echo = FALSE--------------------

ejemp_dic_3 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('opinion_pm|partido_pm',llaves))|>
  filter(!grepl('razon',llaves))



knitr::kable(ejemp_dic_3, caption = 'Tabla 3. Llaves con prefijo "conoce_pm", y "partido_pm"', align = 'c')


## ----ejemplo4, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Especial$candidatoPartido(llave_partido = "partido_pm",
                                                   llave_conocimiento = "conoce_pm",
                                                   respuesta_conoce = "Sí",
                                                   candidatos = c("lia", "javier"),
                                                   corte_otro = 0.0,
                                                   colores_candidatos = c("Javier López Casarín" = "red",
                                                                          "Lía Limón García" = "blue"),
                                                   colores_partido = c("MORENA" = "#A6032F",
                                                                       "PRI" = "#038C33",
                                                                       "Ninguno" = "black",
                                                                       "PAN" = "#0339a6",
                                                                       "Ns/Nc" = "gray60",
                                                                       "Movimiento Ciudadano (MC)" = "#F27405",
                                                                       "Partido Verde (PVEM)" = "#98BF5E",
                                                                       "PT" = "#D91136",
                                                                       "PRD" = "#F2B705",
                                                                       "Otro" = "gray30"),
                                                   cliente = "Javier López Casarín")

