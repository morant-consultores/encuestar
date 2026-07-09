#' Analizar frecuencias de una variable
#'
#' Calcula la media de una variable en un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param pregunta Nombre de la variable a calcular la estimación de proporciones de valores en la base de datos
#' @return Tibble con la media de las estimaciones por valor unico de la variable seleccionada
#' @examples
#' analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "sexo")
#' analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "voto_pr_24")
analizar_frecuencias <- function(diseno, pregunta){
  surveySummary_mean <- survey::svymean(survey::make.formula(pregunta),
                                        design = diseno,
                                        na.rm = TRUE)

  estimacion <-
    surveySummary_mean |>
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media = 2, ee = 3) %>%
    mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                            replacement = "",
                                            string = respuesta),
           pregunta = rlang::expr_text(ensym(pregunta)),
           respuesta = stringr::str_replace_all(respuesta, " \\(No leer\\)", "")) |>
    left_join(surveySummary_mean |>
                stats::confint() %>%
                tibble::as_tibble(rownames = "respuesta") |>
                mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                                        replacement = "",
                                                        string = respuesta),
                       pregunta = rlang::expr_text(ensym(pregunta))) |>
                rename(inf = 2, sup = 3), by = c("respuesta", "pregunta")) |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta,
                                            .x = media,
                                            .fun = max))
  return(estimacion)
}
#' Analizar frecuencias de multiples variables
#'
#' Calcula la media de multiples variables cuyos nombres comparten un patron inicial en comun
#'  de un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param patron_pregunta Cadena de texto en comun (inicial) entre los nombres de las variables a analizar.
#' @param aspectos Vector de tipo cadena de texto (final) que diferencia las variables a analizar.
#'  Separada de patron_pregunta por un guion bajo.
#' @return Base de datos con las estimaciones de frecuencia para cada categoria respondida por cada aspecto distinto.
#' @examples
#' analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "conoce_pm", aspectos = c("astiazaran", "delrio"))
#' analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "conoce_pm", aspectos = c("lia", "javier"))
analizar_frecuencias_aspectos <- function(diseno, diccionario, patron_pregunta, aspectos){
  estimaciones <-
    paste(patron_pregunta, aspectos, sep = "_") %>%
    purrr::map_df(.x = .,
                  .f = ~ analizar_frecuencias(diseno = diseno,
                                              pregunta = .)) |>
    rename(aspecto = pregunta) |>
    relocate(aspecto, .after = sup) |>
    left_join(diccionario |>
                select(aspecto = llaves,
                       pregunta), by = "aspecto") |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta,
                                            .x = media,
                                            .fun = max))
  return(estimaciones)
}
#' Analizar frecuencias a multiples variables asociadas a una misma pregunta con opcion a multirespuesta
#'  de un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param patron_inicial Cadena de texto en comun (inicial) entre los nombres de las variables asociadas a la pregunta
#' @return [tibble()] con las estimaciones de frecuencia para cada valor unico entre las variables
#' @examples
#' analizar_frecuencia_multirespuesta(diseno = encuesta_demo$muestra$diseno, patron_inicial = "medios_com_O")
analizar_frecuencias_multirespuesta <- function(diseno, patron_inicial){
  aux <- diseno$variables %>%
    tibble::rownames_to_column() %>%
    as_tibble() |>
    mutate(weight = weights(diseno)) %>%
    select(rowname, contains(patron_inicial), weight) |>
    pivot_longer(-c(rowname, weight)) %>%
    filter(!is.na(value)) %>%
    mutate(seleccion = 1) %>%
    select(-name) %>%
    pivot_wider(names_from = value, values_from  = seleccion, values_fill = 0) %>%
    select(-rowname) %>% summarise(across(-weight, ~sum(.x*weight))) %>%
    pivot_longer(everything(), names_to = "respuesta", values_to = "value") %>%
    mutate(media = value/sum(weights(diseno)),
           respuesta=forcats::fct_reorder(.f = respuesta,
                                          .x = media,
                                          .fun = max))
  return(aux)
}
#' Analizar cruce entre dos variables
#'  de un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param vartype Parametro de [srvyr::survey_mean()] que reporta la variabilidad de la estimacion
#' @param variable_principal Variable principal que agrupa los resultados de las estimaciones
#' @param variable_secundaria Variable secundaria
#' @param na_rm na_rm=FALSE Variable booleana que indica si se desea filtrar NAs (TRUE) o no (FALSE)
#' @return [tibble()] con las estimaciones de frecuencia para cada valor unico entre las variables
#' @examples
#' analizar_cruce(diseno = encuesta_demo$muestra$diseno,  variable_principal = "sexo",  variable_secundaria = "voto_pr_24", vartype = "cv")
analizar_cruce <- function(diseno, variable_principal, variable_secundaria, vartype,na_rm = FALSE){
  res <-
    srvyr::as_survey_design(diseno) %>%
    {if(na_rm) filter(., !is.na( !!rlang::sym(variable_principal) ), !is.na( !!rlang::sym(variable_secundaria) ) ) else . }|>
  group_by(!!rlang::sym(variable_principal), !!rlang::sym(variable_secundaria)) |>
    summarise(srvyr::survey_mean(na.rm = TRUE, vartype = vartype))
  if(vartype == "cv") {
    res <-
      res |>
      mutate(pres = case_when(`_cv` > .15 & `_cv` < .30 ~ "*",
                              `_cv` > .30 ~ "**",
                              TRUE ~ ""))
  }
  res <-
    res |>
    ungroup()
  return(res)
}
#' Analizar cruce entre multiples variables cuyos nombres compartan valores unicos
#'  de un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param variable_principal Variable principal que agrupa los resultados de las estimaciones
#' @param variables_secundarias Variables secundarias que compartan valores unicos
#' @param filtro_variables_secundarias Valor unico de las variables secundarias de interes a mostrar en resultados
#' @param vartype Parametro de [srvyr::survey_mean()] que reporta la variabilidad de la estimacion
#' @return [tibble()] con las estimaciones de frecuencia para cada valor unico entre las variables
#' @examples
#' analizar_cruce_aspectos(diseno = encuesta_demo$muestra$diseno, variable_principal = "sexo", variables_secundarias = c("conoce_pm_lia", "conoce_pm_javier"), filtro_variables_secundarias = "Sí", vartype = "cv")
analizar_cruce_aspectos <- function(diseno, variable_principal, variables_secundarias, filtro_variables_secundarias, vartype){
  variables_secundarias <- rlang::enquo(variables_secundarias)
  res <-
    srvyr::as_survey_design(diseno) |>
    group_by(!!rlang::sym(variable_principal)) %>%
    summarise(across(!!variables_secundarias, ~ srvyr::survey_mean(.x == !!filtro_variables_secundarias, vartype = vartype, na.rm = TRUE), .names = "{.col}")) |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = -rlang::sym(variable_principal),
                        names_to = "variable",
                        values_to = "valor") |>
    mutate(separar = ifelse(stringr::str_detect(variable, glue::glue('_{vartype}$')), vartype, "mean"),
           variable = stringr::str_remove(variable, glue::glue('_{vartype}'))) |>
    tidyr::pivot_wider(names_from = separar, values_from = valor)
  return(res)
}
#' Calcular datos necesarios para la funcion [graficar_sankey()]
#' @param variables Vector tipo caracter que contiene los nombres de las variables de las cuales
#'  se va a hacer el cruce
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param filtro_var1 Valores de interes de la primer variable presente en el vector
#' @param filtro_var2 Valores de interes de la primer variable presente en el vector
#' @return [tibble()] con las estimaciones de frecuencia para cada valor unico entre las variables
#' @examples
analizar_sankey <- function(diseno, variables, filtro_var1, filtro_var2){
  if(length(variables) == 2) {
    vec_variables <- c(var1 = variables[[1]], var2 = variables[[2]])
  }

  if(length(variables) == 3) {
    vec_variables <- c(var1 = variables[[1]], var2 = variables[[2]], var3 = variables[[3]])
  }
  res <-
    survey::svytable(survey::make.formula(vec_variables),
                     design = diseno) %>%
    tibble::as_tibble()

  if(!is.null(filtro_var1)) {

    res <-
      res |>
      filter(!(!!rlang::sym(variables[[1]]) %in% filtro_var1))

  }

  if(!is.null(filtro_var2)) {

    res <-
      res |>
      filter(!(!!rlang::sym(variables[[2]]) %in% filtro_var2))

  }

  res |>
    ggsankey::make_long(-n, value = n)
}
#' Calcula las estimaciones de asociacion partidista de un personaje en particular
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param llave_partido Patrón que comparten las variables asociadas a las preguntas que relacionan un personaje con un partido político
#' @param llave_conocimiento Patrón que comparten las variables relacionadas a preguntas sobre el conocimiento de un personaje
#' @param respuesta_conoce Filtro sobre el cuál se evalúa la asociación de un personaje a un partído político
#' @param candidatos Vector de nombres cortos asociados a uno o más personajes sobre los cuáles se preguntó su asociación partidista
#' @param corte_otro Parámetro 'prop' de la función 'fct_lump' de la paquetería 'forcats' usado para agrupar valores pequeños de partidos políticos
#' @return Lista de donde cada elemento es un [tibble()] asociado al conocimiento y asociacion partidista
#' @examples
#' \dontrun{
#' # la función es interna (no exportada); el ejemplo requiere encuesta_demo
#' analizar_candidatoPartido(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, llave_partido = "partido_pm", llave_conocimiento = "conoce_pm", respuesta_conoce = "Sí", candidatos = c("lia", "javier"))
#' }
analizar_candidatoPartido <- function(diseno, diccionario, llave_partido, llave_conocimiento, respuesta_conoce, candidatos, corte_otro = 0.05){

  conoce <- paste(llave_conocimiento, candidatos, sep = "_")
  llaves_partido <- paste(llave_partido, candidatos,sep = "_")

  conoce <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_conocimiento, aspectos = candidatos) |>
    filter(respuesta == respuesta_conoce)

  partido <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_partido, aspectos = candidatos) |>
    mutate(respuesta = as.character(respuesta),
           respuesta = forcats::fct_lump(respuesta, w = media, prop = corte_otro, other_level = "Otro")) |>
    count(respuesta, aspecto, wt = media, name = "media") %>%
    group_by(aspecto) |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta, .x = media, .fun = max)) |>
    ungroup() |>
    arrange(aspecto, desc(media)) |>
    group_by(aspecto) %>%
    mutate(sup = cumsum(media),
           inf = lag(sup, default = 0),
           label = (inf +sup)/2)

  bases <- list(conoce = conoce, partido = partido)

  return(list(conoce = conoce, partido = partido))
}
#' Calcular el saldo de opinión por personaje
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param candidatos Cadena de texto que diferencia las variables a analizar. Separada de patron_pregunta por un guion bajo.
#' @param llave_opinion Patrón que comparten las variables asociadas a las preguntas sobre la opinión pública hacia un personaje.
#' @param grupo_positivo Conjunto de valores de la variable opinión tratados como positivos.
#' @param grupo_negativo Conjunto de valores de la variable opinión tratados como negativos
#' @examples
#' analizar_saldoOpinion(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, llave_opinion = "opinion_pm", candidatos = c("astiazaran", "delrio"), grupo_positivo = c("Muy buena", "Buena"), grupo_negativo = c("Muy mala", "Mala"))
analizar_saldoOpinion <- function(diseno, diccionario, llave_opinion, candidatos, grupo_positivo, grupo_negativo){

  llave_op = paste(llave_opinion) # BUG

  bd <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_op, aspectos = candidatos) |>
    left_join(diccionario |> select(aspecto = llaves, tema), by = "aspecto")

  res <- bd %>%
    mutate(!!rlang::sym(llave_op) := case_when(respuesta %in% grupo_positivo ~"Positiva",
                                               respuesta %in% grupo_negativo ~"Negativa",)) %>%
    filter(!is.na(!!rlang::sym(llave_op))) %>%
    mutate(persona = stringr::str_replace(aspecto, glue::glue("{llave_op}_"), "")) %>%
    count(persona, tema, grupo = !!rlang::sym(llave_op), wt = media, name = "saldo") %>%
    mutate(saldo = if_else(grupo == "Negativa", -saldo, saldo))

  return(res)
}
#' Calcular media movil de una variable
#'  de un diseno muestral construido con la paqueteria `survey`
#'
#' @param bd_resultados [tibble()] que contien la variable de interes y los pesos necesarios para el calculo
#' @param variable Nombre de la variable de interes
#' @param sin_peso Logical. Se usa para calcular resultados con o sin pesos
#' @param valores_interes Valor de interes contenido en la variable de interes
#'
#' @return [tibble()] con las variables: hora, variable_interes, movil_variable_nteres
#' @examples
#' calcular_mediaMovil(bd_resultados = encuesta_demo$Resultados$Tendencias$bd_resultados, variable = "voto_pr_24", valores_interes = "Claudia Sheinbaum por MORENA-PT-Partido Verde", sin_peso = TRUE)
#' calcular_mediaMovil(bd_resultados = encuesta_demo$Resultados$Tendencias$bd_resultados, variable = "voto_pr_24", valores_interes = "Claudia Sheinbaum por MORENA-PT-Partido Verde", sin_peso = FALSE)
calcular_mediaMovil <- function(bd_resultados, variable, valores_interes, sin_peso) {
    res <- bd_resultados %>%
    {
      if(sin_peso) {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable))
      } else {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable), wt = peso)|>
          left_join(
            count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable),name = 'n_snp'),
            by= c('hora',variable)
          )
      }
    }  %>%
    group_by(hora) |>
    complete(!!rlang::sym(variable) := valores_interes,
             fill = list(n = 0))  %>%
      {if(!sin_peso) complete(., !!rlang::sym(variable) := valores_interes,
                              fill = list(n_snp = 0)) else .}|>
    ungroup() |>
    mutate(tot = sum(n), .by = c(hora)) |>
    mutate(n_acum = cumsum(n),
           tot_acum = cumsum(tot), .by = c(!!rlang::sym(variable)),
           !!rlang::sym(paste0("movil_", variable)) := n_acum/tot_acum)

  pos_gen <- res|>
    mutate(tot_gen = cumsum(n),
           tot_gen_max = max(tot_gen),
           tot_gen_pos = ifelse(tot_gen >= max(tot_gen)/2,1,0),
           pos_aux_gen = !duplicated(tot_gen_pos),
           pos_gen = if_else(pos_aux_gen == TRUE & tot_gen_pos ==  1, hora, NA) ) %>%
    filter(!is.na(pos_gen))|>
    pull(pos_gen)

  if(!sin_peso){
    pos_gen_snp <- res|>
      mutate(tot_gen_snp = cumsum(n_snp),
             tot_gen_max_snp = max(tot_gen_snp),
             tot_gen_pos_snp = ifelse(tot_gen_snp >= max(tot_gen_snp)/2,1,0),
             pos_aux_gen_snp = !duplicated(tot_gen_pos_snp),
             pos_gen_snp = if_else(pos_aux_gen_snp == TRUE & tot_gen_pos_snp ==  1, hora, NA) )|>
      filter(!is.na(pos_gen_snp))|>
      pull(pos_gen_snp)
  }

  res|>
    mutate(pos_gen=pos_gen) %>%
    {if(!sin_peso) mutate(.,pos_gen_snp = pos_gen_snp) else .} |>
    filter(!!rlang::sym(variable) %in% valores_interes)  %>%
    {if(sin_peso)  select(.data = .,hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)),pos_gen)
      else select(.data = .,hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)),pos_gen,pos_gen_snp)}


}

#' Calcular media movil de una variable agrupado por region
#'  de un diseno muestral construido con la paqueteria `survey`
#'
#' @param bd_resultados [tibble()] que contien la variable de interes y los pesos necesarios para el calculo
#' @param variable Nombre de la variable de interes
#' @param sin_peso Logical. Se usa para calcular resultados con o sin pesos
#' @param valores_interes Valor de interes contenido en la variable de interes
#' @param variable_region Nombre de la variable que indica las regiones en el diseno muestral.
#'
#' @return [tibble()] con las variables: hora, variable_interes, movil_variable_nteres
#'
#' @examples
#' calcular_mediaMovil_region(bd_resultados = encuesta_demo$Resultados$Tendencias$bd_resultados, variable = "voto_pr_24", valores_interes = "Claudia Sheinbaum por MORENA-PT-Partido Verde", variable_region = "region", sin_peso = TRUE)
#' calcular_mediaMovil_region(bd_resultados = encuesta_demo$Resultados$Tendencias$bd_resultados, variable = "voto_pr_24", valores_interes = "Claudia Sheinbaum por MORENA-PT-Partido Verde", variable_region = "region", sin_peso = FALSE)
calcular_mediaMovil_region <- function(bd_resultados, variable, valores_interes, variable_region, sin_peso) {
  res <- bd_resultados %>%
    {
      if(sin_peso) {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable_region), !!rlang::sym(variable))
      } else {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable_region), !!rlang::sym(variable), wt = peso)|>
          left_join(
            count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable_region), !!rlang::sym(variable),name = 'n_snp'),
            by= c('hora',variable,variable_region)
          )
      }
    }  %>%
    group_by(hora, !!rlang::sym(variable_region)) |>
    complete(!!rlang::sym(variable) := valores_interes,
             fill = list(n = 0))  %>%
    {if(!sin_peso) complete(., !!rlang::sym(variable) := valores_interes,
                            fill = list(n_snp = 0)) else .}|>
    ungroup() |>
    mutate(tot = sum(n), .by = c(hora, !!rlang::sym(variable_region) )) |>
    mutate(n_acum = cumsum(n),
           tot_acum = cumsum(tot), .by = c(!!rlang::sym(variable_region), !!rlang::sym(variable)),
           !!rlang::sym(paste0("movil_", variable)) := n_acum/tot_acum)

  pos_gen <- res|>
    mutate(tot_gen = cumsum(n),
           tot_gen_max = max(tot_gen),
           tot_gen_pos = ifelse(tot_gen >= max(tot_gen)/2,1,0),
           pos_aux_gen = !duplicated(tot_gen_pos),
           pos_gen = if_else(pos_aux_gen == TRUE & tot_gen_pos ==  1, hora, NA) ) %>%
    filter(!is.na(pos_gen))|>
    pull(pos_gen)


  res<- res|>
    left_join(res|>
                group_by(!!rlang::sym(variable_region))|>
                mutate(tot_gen = cumsum(n),
                       tot_gen_max = max(tot_gen),
                       tot_gen_pos = ifelse(tot_gen >= max(tot_gen)/2,1,0),
                       pos_aux_gen = !duplicated(tot_gen_pos),
                       pos_reg_gen = if_else(pos_aux_gen == TRUE & tot_gen_pos ==  1, hora, NA) )|>
                tidyr::fill(pos_reg_gen,.direction = 'updown')|>
                ungroup()|>
                distinct(!!rlang::sym(variable_region),pos_reg_gen),
              by = c(variable_region))


  if(!sin_peso){
    pos_gen_snp <- res|>
      mutate(tot_gen_snp = cumsum(n_snp),
             tot_gen_max_snp = max(tot_gen_snp),
             tot_gen_pos_snp = ifelse(tot_gen_snp >= max(tot_gen_snp)/2,1,0),
             pos_aux_gen_snp = !duplicated(tot_gen_pos_snp),
             pos_gen_snp = if_else(pos_aux_gen_snp == TRUE & tot_gen_pos_snp ==  1, hora, NA) )|>
      filter(!is.na(pos_gen_snp))|>
      pull(pos_gen_snp)


    res<- res|>
      left_join(res|>
                  group_by(!!rlang::sym(variable_region))|>
                  mutate(tot_gen_snp = cumsum(n_snp),
                         tot_gen_max_snp = max(tot_gen_snp),
                         tot_gen_pos_snp = ifelse(tot_gen_snp >= max(tot_gen_snp)/2,1,0),
                         pos_aux_gen_snp = !duplicated(tot_gen_pos_snp),
                         pos_reg_gen_snp = if_else(pos_aux_gen_snp == TRUE & tot_gen_pos_snp ==  1, hora, NA) )|>
                  tidyr::fill(pos_reg_gen_snp,.direction = 'updown')|>
                  ungroup()|>
                  distinct(!!rlang::sym(variable_region),pos_reg_gen_snp),
                by = c(variable_region))
  }

  res|>
    mutate(pos_gen=pos_gen) %>%
    {if(!sin_peso) mutate(.,pos_gen_snp = pos_gen_snp) else .} |>
    filter(!!rlang::sym(variable) %in% valores_interes)  %>%
    {if(sin_peso)  select(.data = .,!!rlang::sym(variable_region),hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)),pos_gen, pos_reg_gen)
      else select(.data = .,!!rlang::sym(variable_region),hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)),pos_gen, pos_reg_gen, pos_gen_snp, pos_reg_gen_snp)}

}
#'  Calcular resultados de opinion por candidato en formato tabla
#'
#'  Produce y ordena los resultados necesarios para la funcion [formatear_tabla_candidatoOpinion()]
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param patron_opinion Cadena de texto que contiene el patron en comun (inicial) entre las variables asociadas a opinion
#' @param patron_conocimiento Cadena de texto que contiene el patron en comun (inicial) entre las variables asociadas a opinion
#' @param aspectos Vector de tipo cadena de texto que diferencia las variables de interes
#' @param filtro_conocimiento Cadena de texto usada para filtrar valores de interes asociado a preguntas con pase en el cuestionario
#' @param orden_opinion Vector ordenado tipo caracter usado para establecer el orden de los posibles valores unicos en las variables de opinion
#' @param ns_nc Cadena de texto asociada a las respues en que No sabe o No contesta
#' @param salto_respuestas Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos los encabezados de la tabla
#'
#' @return [tibble()] con las variables necesarias para la funcion [formatear_tabla_candidatoOpinion()]
#'
#' @examples
#' calcular_tabla_candidatoOpinion(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_opinion = "opinion_pm", patron_conocimiento = "conoce_pm", aspectos = c("astiazaran", "delrio"), filtro_conocimiento = "respuesta == 'Sí'", orden_opinion = c("Muy mala", "Mala", "Regular", "Buena", "Muy buena"), ns_nc =  "Ns/Nc", salto_respuestas = 60)
calcular_tabla_candidatoOpinion <- function(diseno, diccionario, patron_opinion, patron_conocimiento, aspectos, filtro_conocimiento, orden_opinion, ns_nc, salto_respuestas) {

  bd_opinion <-
    analizar_frecuencias_aspectos(diseno = diseno,
                                  diccionario = diccionario,
                                  patron_pregunta = patron_opinion,
                                  aspectos = aspectos) |>
    left_join(diccionario %>% select(aspecto = llaves, tema)) |>
    tidyr::pivot_wider(id_cols = tema,
                       names_from = respuesta,
                       values_from = media) |>
    select(Candidato = tema, all_of(orden_opinion), ns_nc) |>
    rename_with(.fn = ~ stringr::str_wrap(string = .x,
                                          width = salto_respuestas),
                .cols = all_of(orden_opinion))

  if(!is.na(patron_conocimiento)) {
    bd_conocimiento <-
      analizar_frecuencias_aspectos(diseno = diseno,
                                    diccionario = diccionario,
                                    patron_pregunta = patron_conocimiento,
                                    aspectos = aspectos) %>%
      filter(eval(rlang::parse_expr(filtro_conocimiento))) %>%
      left_join(diccionario %>% select(aspecto = llaves, tema)) |>
      select(tema, Conocimiento = media)
  }

  bd_opinion %>%
    {
      if(!is.na(patron_conocimiento)) {
        left_join(x = ., y = bd_conocimiento, by = c("Candidato" = "tema")) |>
          arrange(desc(Conocimiento))
      } else {
        arrange(., desc(.data[[names(.)[2]]]))
      }
    } %>%
    mutate(across(.cols = !Candidato, .fns = ~ scales::percent(.x, accuracy = 1.)))

}
#' Tabla de voto cruzado
#'
#' Calcular y ordena los resultados de un cruce entre una variable principal y una variable secundaria.
#'  Muestra los valores indicados en el filtro de la segunda variable. Produce la base de datos
#'  necesaria para la funcion [formatear_tabla_votoCruzado()]
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param var1 Variable principal para hacer el cruce
#' @param var2 Variable secundaria para hacer el cruce
#' @param filtro_var2 Valores unicos de la variable secundaria que son de interes
#' @param na_rm na_rm=TRUE Variable booleana que indica si se desea filtrar NAs (TRUE) o no (FALSE)
#'
#' @return [tibble()] con las variables necesarias para la funcion [formatear_tabla_votoCruzado()]
#'
#' @examples
#' calcular_tabla_votoCruzado(diseno = encuesta_demo$muestra$diseno, var1 = "voto_pm_24", var2 = "voto_pr_24", filtro_var2 = c("Claudia Sheinbaum por MORENA-PT-Partido Verde", "Xóchitl Gálvez por PAN-PRI-PRD"))
calcular_tabla_votoCruzado = function(diseno, var1, var2, filtro_var2,na_rm = TRUE){

  orden_var1 <-
    analizar_frecuencias(diseno = diseno,
                                     pregunta = var1) |>
    arrange(desc(media)) |>
    mutate(respuesta = as.character(respuesta)) |>
    select(!!rlang::sym(var1) := respuesta, media)

  orden_var2 <-
    analizar_frecuencias(diseno = diseno,
                                     pregunta = var2) |>
    arrange(desc(media)) %>%
    {
      if(!is.null(filtro_var2)) {
        filter(.data = ., respuesta %in% filtro_var2)
      } else {
        .
      }
    } %>%
    mutate(respuesta = as.character(respuesta)) |>
    pull(respuesta)

  aux <-
    analizar_cruce(diseno = diseno,
                               variable_principal = var1,
                               variable_secundaria = var2,
                               vartype = "cv",
                   na_rm = na_rm) |>
    ungroup() |>
    select(var1, var2, coef) |>
    tidyr::pivot_wider(id_cols = var1,
                       names_from = var2,
                       values_from = coef) |>
    left_join(orden_var1) |>
    arrange(desc(media)) |>
    select(!media) |>
    mutate(across(.cols = !var1, .fns = ~ tidyr::replace_na(data = .x, replace = 0)),
           across(.cols = !var1, .fns = ~ scales::percent(x = .x, accuracy = 1.))) |>
    select(var1, all_of(orden_var2))
  return(aux)
}
#' Title
#'
#' @param bd
#' @param vars
#' @param stimuli
#'
#' @return
#'
#' @examples
analizar_blackbox_1d <- function(bd, vars, stimuli){
  stmli <- bd %>% select(stimuli =all_of(stimuli))
  basic <- bd %>% select(all_of(vars))
  nas <- basic %>% mutate(across(everything(), ~if_else(is.na(.x), 9999,.x)))

  issue <- basicspace::blackbox(nas, missing=c(9999),verbose=FALSE,dims=1,minscale=ncol(nas)/2+1)

  individuals <- issue$individuals %>% pluck(1) %>% as_tibble %>%
    bind_cols(stmli)

  orden <- individuals %>% group_by(stimuli = all_of(stimuli)) %>%
    summarise(media = mean(c1,na.rm = T)) %>% arrange(desc(media))

  individuals <- individuals %>% mutate(stimuli = factor(x = stimuli,levels =  orden %>% pull(stimuli)), stimuli*-1)
  return(
    list(
      stimuli = issue$stimuli %>% pluck(1),
      fits = issue$fits,
      individuals = individuals,
      slf = orden
    )
  )
}
#' Title
#'
#' @param var1
#' @param var2
#' @param legenda1
#' @param legenda2
#' @param diseno
#' @param colores
#'
#' @return
#'
#' @examples
analisis_correspondencia <- function(var1, var2, legenda1=NULL, legenda2=NULL, diseno, colores =NULL){

  if(is.null(legenda1)) legenda1 <- var1
  if(is.null(legenda2)) legenda2 <- var2
  if(is.null(colores)) colores <- c("#DE6400","#023047")

  formula <- survey::make.formula(c(var1,var2))
  aux <- survey::svytable(formula, design = diseno) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = var2, values_from = "n") %>%
    tibble::column_to_rownames(var = var1)


  # chisq.test(aux)
  res.ca <- FactoMineR::CA(aux, graph = F)
  eig <- factoextra::get_eigenvalue(res.ca)[, 2]

  res.ca$col$coord %>%
    as_tibble(rownames = "respuesta") %>%
    janitor::clean_names() %>%
    select(respuesta,num_range("dim_",1:2)) %>%
    mutate(variable = legenda2) %>%
    bind_rows(res.ca$row$coord %>%
                as_tibble(rownames = "respuesta") %>%
                janitor::clean_names() %>%
                select(respuesta,num_range("dim_",1:2)) %>%
                mutate(variable = legenda1)) %>%
    ggpubr::ggscatter(x = "dim_1", y = "dim_2", color = "variable", label = "respuesta", repel = T) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = scales::percent(eig[1]/100,accuracy = .1),
         y = scales::percent(eig[2]/100,accuracy = .1),
         color = ""
    ) +
    scale_color_manual(values = colores) +
    lemon::scale_x_symmetric() +
    lemon::scale_y_symmetric() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#C5C5C5",linetype = "dotted"),
          panel.grid.major.y = element_line(colour = "#C5C5C5",linetype = "dotted"))

}
#' Analizar metodología de MORENA (EN DESUSO)
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param personajes Vector de nombres cortos asociados a uno o más personajes sobre los cuáles se preguntó la batería de MORENA
#' @param atributos Tibble que contiene los atributos y los puntos que obtiene el ganador de cada atributo
#'
#' @return
#'
#' @examples
#' analizar_morena(diseno = diseno, diccionario = diccionario, personajes = c("era", "sasil"), atributos = atributos)
#' analizar_morena(diseno = diseno, diccionario = diccionario, personajes = c("era", "sasil", "jaac"), atributos = atributos)
analizar_morena <- function(diseno, diccionario, personajes, atributos){

  #opinión positiva
  o_p <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "opinion", aspectos = personajes) %>%
    filter(respuesta == "Buena") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo", "personaje"), remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #atributos
  atr_p <- atributos %>%
    purrr::pmap_df(function(atributo, puntos){
      analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = atributo, aspectos = personajes) %>%
        filter(respuesta %in% c("Mucho", "Algo")) %>%
        mutate(media = if_else(respuesta == "Algo", media *.5,media)) %>%
        group_by(aspecto) %>%
        summarise(media = sum(media)) %>%
        mutate(ganador = media == max(media),
               puntos = if_else(ganador, puntos, 0)) %>%
        separate(aspecto, c("atributo", "personaje"), remove = F)
    })

  #buen candidato
  cand <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "buencandidato", aspectos = personajes) %>%
    filter(respuesta == "Sí") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 1, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #votaria
  voto <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "votaria", aspectos = personajes) %>%
    filter(respuesta == "Sí votaría") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #preferencia
  pref <- analizar_frecuencias(diseno = diseno, pregunta = "candidato_preferencia") %>%
    filter(!respuesta %in% c("Ns/Nc", "Ninguno", "Otro")) %>%
    mutate(ganador = media == max(media),
           puntos = if_else(ganador, 2.75, 0)) %>%
    transmute(atributo = "preferencia", tema = respuesta, media, ganador, puntos)

  #juntar
  atr <- o_p %>%
    bind_rows(atr_p) %>%
    bind_rows(cand) %>%
    bind_rows(voto) %>%
    left_join(diccionario %>% select(llaves, tema), by = c("aspecto" = "llaves")) %>%
    bind_rows(pref)
  return(atr)
}
#' Calcular valor más frecuente por región de acuerdo al diseño muestral
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param regiones Shape file de las regiones-estrato de la encuesta
#' @param lugar Primer lugar del promedio por región de la variable "variable" usando la función dense_rank
#' @param variable Variable a obtener la estimación estimación por región
#' @param lugar Lugar del top de las estimaciones por región
#' @return
#' @examples
#' calcular_ganadorRegion(regiones = shp, variable = "voto_partido", lugar = lugar, diseno = diseno)
calcular_ganadorRegion <- function(diseno, regiones, variable, lugar){
  bd_topEstimaciones <- analizar_frecuenciasRegion(regiones = regiones, variable = variable, diseno = diseno) |>
    janitor::clean_names() |>
    as_tibble() |>
    select(!c(geometry, n)) |>
    select(!contains("se")) |>
    tidyr::pivot_longer(cols = !region, names_to = rlang::expr_text(ensym(variable)), values_to = "estimacion") |>
    group_by(region) %>%
    filter(dense_rank(-estimacion) == lugar)
  tbl <- regiones %>%
    left_join(bd_topEstimaciones, by = "region")
  return(tbl)
}
