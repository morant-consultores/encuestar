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

## ----ejemplo1,  fig.width = 14, fig.height = 7.56, echo = TRUE----------------
encuesta_demo$Resultados$Tendencias$intencion_voto(variable = "voto_pr_24",
                                                   valores_interes = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                       "Xóchitl Gálvez por PAN-PRI-PRD"),
                                                   colores = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                               "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                                   sin_peso = FALSE,
                                                   linea_peso = F)


## ----dicc_ejemp_1,warning=FALSE,message=FALSE,echo = FALSE--------------------
library(dplyr)
ejemp_dic_1 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('conoce_pm',llaves))|>
  filter(!grepl('razon|lia|javier',llaves))



knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm"', align = 'c')


## ----ejemplo2,  fig.width = 14, fig.height = 7.56, echo = TRUE----------------
encuesta_demo$Resultados$Tendencias$conocimiento(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), 
                                                 colores = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"),
                                                 sin_peso = FALSE, 
                                                 valores_interes = "Sí",
                                                 linea_peso = F)


## ----ejemplo3,  fig.width = 14, fig.height = 7.56, echo = TRUE----------------
encuesta_demo$Resultados$Tendencias$intencion_voto_region(variable = "voto_pr_24",
                                                   valores_interes = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                       "Xóchitl Gálvez por PAN-PRI-PRD"),
                                                   colores = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                               "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                                   variable_region = "region", 
                                                   sin_peso = FALSE,
                                                   linea_peso = F)


## ----dicc_ejemp_1_1,warning=FALSE,message=FALSE,echo = FALSE------------------

knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm"', align = 'c')


## ----ejemplo4,  fig.width = 14, fig.height = 7.56, echo = TRUE----------------
encuesta_demo$Resultados$Tendencias$conocimiento_region(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), 
                                                 colores = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"),
                                                 variable_region = "region",
                                                 sin_peso = FALSE, 
                                                 valores_interes = "Sí",
                                                 linea_peso = F)


