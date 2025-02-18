## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE, 
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(encuestar)

## ----ejemplo1, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Cruce$lineas(variable_principal = "region",
                                      variable_secundaria = "voto_pr_24",
                                      orden_variable_principal = c("Perdidas", "Competitivas", "Voto Blando", "Voto Duro"),
                                      valores_variable_secundaria = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                      "Xóchitl Gálvez por PAN-PRI-PRD"),
                                      colores_variable_secundaria = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                                      "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                      limits = c(0, 0.5))


## ----ejemplo2, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Cruce$bloques(variable_principal = "sexo",
                                       variable_secundaria = "identificacion_partido",
                                       colores_variable_secundaria = c("MORENA" = "#A6032F",
                                                                       "PRI" = "#038C33",
                                                                       "Ninguno" = "black",
                                                                       "PAN" = "#0339a6",
                                                                       "Ns/Nc" = "gray60",
                                                                       "Movimiento Ciudadano (MC)" = "#F27405",
                                                                       "Partido Verde (PVEM)" = "#98BF5E",
                                                                       "PT" = "#D91136",
                                                                       "PRD" = "#F2B705",
                                                                       "Otro" = "gray30"))

## ----ejemplo3, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Cruce$sankey_categorica(variables = c("sexo", "identificacion_partido"),
                                                 colores = c("MORENA" = "#A6032F",
                                                             "PRI" = "#038C33",
                                                             "Ninguno" = "black",
                                                             "PAN" = "#0339a6",
                                                             "Ns/Nc" = "gray60",
                                                             "Movimiento Ciudadano (MC)" = "#F27405",
                                                             "Partido Verde (PVEM)" = "#98BF5E",
                                                             "PT" = "#D91136",
                                                             "PRD" = "#F2B705",
                                                             "Otro" = "gray30",
                                                             "F" = "pink",
                                                             "M" = "blue"))

## ----ejemplo4, fig.width = 10, fig.height = 10, echo = TRUE-------------------
encuesta_demo$Resultados$Cruce$tabla_votoCruzado(var1 = "sexo",
                                                 var2 = "identificacion_partido",
                                                 filtro_var2 = c("MORENA", "PRI", "PAN"),
                                                 etiquetas = c("Sexo", "Identificacion"),
                                                 colores_var1 = c("F" = "pink",
                                                                  "M" = "blue"),
                                                 colores_var2 = c("MORENA" = "#A6032F",
                                                                  "PRI" = "#038C33",
                                                                  "PAN" = "#0339a6"))

## ----cruce unitario, fig.width = 10, fig.height = 10, echo = TRUE-------------

encuesta_demo$Resultados$Cruce$lolipop_diferencias(variable_principal = "sexo", 
                                                   variables_secundarias = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
                                                   filtro_variables_secundarias = "Sí", 
                                                   orden_variablePrincipal = c("M", "F"), 
                                                   colores_variables_secundarias = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"), 
                                                   caption = "Intención de voto por sexo", 
                                                   wrap_caption = 40) +
  ggplot2::scale_x_discrete(labels = c("F" = "Mujeres",
                              "M" = "Hombres"))


## ----cruce unitario_reversible, fig.width = 10, fig.height = 10, echo = TRUE----

encuesta_demo$Resultados$Cruce$lolipop_diferencias(variable_principal = "sexo", 
                                                   variables_secundarias = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
                                                   filtro_variables_secundarias = "Sí", 
                                                   orden_variablePrincipal = c("Antonio \"Toño\" Astiazarán", 
                                                                                     "María Dolores Del Río"), 
                                                   colores_variables_secundarias = c("M" = "blue", 
                                                                                     "F" = "pink"), 
                                                   caption = "Intención de voto por sexo", 
                                                   invertir_variables = TRUE,
                                                   wrap_caption = 40)

