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

## ----ejemplo1, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Cruce$lineas(variable_principal = "region",
                                      variable_secundaria = "voto_pr_24",
                                      orden_variable_principal = c("Perdidas", "Competitivas", "Voto Blando", "Voto Duro"),
                                      valores_variable_secundaria = c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
                                                                      "Xóchitl Gálvez por PAN-PRI-PRD"),
                                      colores_variable_secundaria = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
                                                                      "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"), 
                                      limits = c(0, 0.9))


## ----lineas muestra, echo = TRUE, eval=FALSE----------------------------------
#  encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno,
#                             variable_principal = "region",
#                             variable_secundaria = "voto_pr_24",
#                             vartype = "cv",
#                             na_rm = T) |>
#    filter(voto_pr_24 %in% c("Claudia Sheinbaum por MORENA-PT-Partido Verde",
#                             "Xóchitl Gálvez por PAN-PRI-PRD")) |>
#    rename(var_x = "region",
#           var_y = "voto_pr_24",
#           media = coef)|>
#    encuestar:::graficar_lineas(orden_var_x = c("Perdidas", "Competitivas", "Voto Blando", "Voto Duro"),
#                    colores = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F",
#                                "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6"),
#                    limits = c(0,0.9)) +
#    encuestar::tema_morant()

## ----ejemplo2, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
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

## ----ejemplo2.1, fig.width = 14, fig.height = 7.56, echo = TRUE---------------
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
                                                                       "Otro" = "gray30"), 
                                       filter_variable_secundaria = c("MORENA", "PRI"))

## ----bloques muestra, echo = TRUE, eval=FALSE---------------------------------
#  encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno,
#                             variable_principal = "sexo",
#                             variable_secundaria = "identificacion_partido",
#                             vartype = "cv",
#                             na_rm = T) |>
#    filter(identificacion_partido %in% c("MORENA", "PRI") ) |>
#    encuestar:::graficar_cruce_bloques(cruce = "sexo",
#                                       variable = "identificacion_partido",
#                                       vartype =  "cv",
#                                       colores_variable_secundaria = c("MORENA" = "#A6032F",
#                                                                       "PRI" = "#038C33",
#                                                                       "Ninguno" = "black",
#                                                                       "PAN" = "#0339a6",
#                                                                       "Ns/Nc" = "gray60",
#                                                                       "Movimiento Ciudadano (MC)" = "#F27405",
#                                                                       "Partido Verde (PVEM)" = "#98BF5E",
#                                                                       "PT" = "#D91136",
#                                                                       "PRD" = "#F2B705",
#                                                                       "Otro" = "gray30"))+
#    encuestar::tema_morant()

## ----ejemplo3, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
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

## ----Sankey muestra, echo = TRUE, eval=FALSE----------------------------------
#  encuestar:::analizar_sankey(diseno =  encuesta_demo$muestra$diseno,
#                  variables = c("sexo", "identificacion_partido"),
#                  filtro_var1 = NULL,
#                  filtro_var2 = NULL) |>
#    encuestar:::graficar_sankey(
#                    variables = c("sexo", "identificacion_partido"),
#                    size_text_cat = 9,
#                    colores = c("MORENA" = "#A6032F",
#                                "PRI" = "#038C33",
#                                "Ninguno" = "black",
#                                "PAN" = "#0339a6",
#                                "Ns/Nc" = "gray60",
#                                "Movimiento Ciudadano (MC)" = "#F27405",
#                                "Partido Verde (PVEM)" = "#98BF5E",
#                                "PT" = "#D91136",
#                                "PRD" = "#F2B705",
#                                "Otro" = "gray30",
#                                "F" = "pink",
#                                "M" = "blue"))

## ----ejemplo4, fig.width = 14, fig.height = 7.56, echo = TRUE-----------------
encuesta_demo$Resultados$Cruce$tabla_votoCruzado(var1 = "sexo",
                                                 var2 = "identificacion_partido",
                                                 filtro_var2 = c("MORENA", "PRI", "PAN"),
                                                 etiquetas = c("Sexo", "Identificacion"),
                                                 colores_var1 = c("F" = "pink",
                                                                  "M" = "blue"),
                                                 colores_var2 = c("MORENA" = "#A6032F",
                                                                  "PRI" = "#038C33",
                                                                  "PAN" = "#0339a6"))

## ----Tabla muestra, echo = TRUE, eval=FALSE-----------------------------------
#  encuestar:::calcular_tabla_votoCruzado(diseno = encuesta_demo$muestra$diseno,
#                             var1 = "sexo",
#                             var2 = "identificacion_partido",
#                             filtro_var2 = c("MORENA", "PRI", "PAN"),
#                             na_rm = T) |>
#    encuestar:::formatear_tabla_votoCruzado(var1 = "sexo",
#                                var2 = "identificacion_partido",
#                                filtro_var2 = c("MORENA", "PRI", "PAN"),
#                                etiquetas = c("Sexo", "Identificacion"),
#                                colores_var1 = c("F" = "pink",
#                                                 "M" = "blue"),
#                                colores_var2 = c("MORENA" = "#A6032F",
#                                                "PRI" = "#038C33",
#                                                "PAN" = "#0339a6"),
#                                size_text_header = 10,
#                                size_text_body = 10)

## ----dicc_ejemp_1,warning=FALSE,message=FALSE,echo = FALSE--------------------
library(dplyr)
ejemp_dic_1 <- encuesta_demo$cuestionario$diccionario|>
  select(tipo_pregunta,llaves,tema,respuestas)|>
  filter(grepl('sexo',llaves)|grepl('conoce_pm',llaves))



knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Llaves con prefijo "conoce_pm" ', align = 'c')


## ----cruce unitario, fig.width = 14, fig.height = 7.56, echo = TRUE-----------

encuesta_demo$Resultados$Cruce$lolipop_diferencias(variable_principal = "sexo", 
                                                   variables_secundarias = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
                                                   filtro_variables_secundarias = "Sí", 
                                                   orden_variablePrincipal = c("M", "F"), 
                                                   colores_variables_secundarias = c("Antonio \"Toño\" Astiazarán" = "green", 
                                                                                     "María Dolores Del Río" = "blue"), 
                                                   caption = "Intención de voto por sexo", 
                                                   wrap_caption = 40,ver_diferencias = FALSE) +
  ggplot2::scale_x_discrete(labels = c("F" = "Mujeres",
                              "M" = "Hombres"))


## ----cruce unitario_reversible, fig.width = 14, fig.height = 7.56, echo = TRUE----

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

