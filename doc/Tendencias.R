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

## ----ejemplo1, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Tendencias$intencion_voto(variable = "voto_pr_24",
                                                   valores_interes = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                       "Xóchitl Gálvez por PAN-PRI-PRD"),
                                                   colores = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                               "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                                   sin_peso = FALSE)


## ----ejemplo2, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Tendencias$conocimiento(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), 
                                                 colores = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"),
                                                 sin_peso = FALSE, 
                                                 valores_interes = "Sí")


## ----ejemplo3, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Tendencias$intencion_voto_region(variable = "voto_pr_24",
                                                   valores_interes = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                       "Xóchitl Gálvez por PAN-PRI-PRD"),
                                                   colores = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                               "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                                   variable_region = "region", 
                                                   sin_peso = FALSE)


## ----ejemplo4, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Tendencias$conocimiento_region(variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"), 
                                                 colores = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"),
                                                 variable_region = "region",
                                                 sin_peso = FALSE, 
                                                 valores_interes = "Sí")


