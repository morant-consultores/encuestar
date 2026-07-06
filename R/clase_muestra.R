#' Clase Muestra
#'
#' @description La clase `Muestra` recibe las respuestas estandarizadas de las entrevistas
#'  levantadas en campo y calcula los pesos de los individuos de acuerdo al cluster donde
#'  es asignada la entrevista, la edad y el sexo. También provee métodos para modificar el
#'  diseno muestral de la clase para producir resultados con diferentes ponderaciones.
#'
#' @field muestra Campo de la clase `Encuesta`. Contiene toda la información necesaria para
#'  calcular los pesos asignados a los individuos de acuerdo al proceso de muestreo.
#' @field base [tibble()] que contiene la informacion de los factores de correccion poblacional
#'  de cada cluster
#' @field diseno_original Diseno canonico u original de la encuesta
#' @field region Region (o estrato) del diseno actual de la encuesta. Es uno de los valores únicos
#'  de la variable `region` creada durante el diseno muestral. Dicha variable actúa como estrato
#'  del marco muestral.
#' @field calibracion Nombre de la calibración actualmente aplicada al diseno muestral.
#' @field diseno Objeto tipo `design` de la paquetería [survey]
#' @field calibraciones Lista de distintas calibraciones que se han agregado durante la produccion
#'  a la clase.
#'
#' @description La clase `Muestra` recibe.
Muestra <-
  R6::R6Class(
    classname = "Muestra",
    public = list(
      muestra = NULL,
      base = NULL,
      diseno_original = NULL,
      region = NULL,
      calibracion = NULL,
      diseno = NULL,
      calibraciones = NULL,
      #' @description Se crean y calculan las variables y los pesos relacionados a los individuos
      #'  entrevistados.
      #' @param muestra Campo heredado de la clase `Encuesta`.
      #' @param respuestas [tibble()] que contiene las respuestas de los individuos entrevistados
      #'  en campo y que ha sido procesado por la clase `Respuestas`.
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro.
      #' @param var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      initialize = function(muestra, respuestas, nivel, var_n){
        self$muestra <- muestra
        self$base <- muestra$muestra %>% purrr::pluck(var_n)
        self$recalcular_fpc(respuestas = respuestas, nivel, var_n)
      },
      #' @description Recalcula los factores de correccion poblacional de acuerdo a los individuos
      #'  entrevistados en campo
      #' @param respuestas [tibble()] que contiene las respuestas de los individuos entrevistados
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro.
      #' @param var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      recalcular_fpc = function(respuestas, nivel, var_n){
        var_pob <- self$muestra$variable_poblacional

        pob <- self$base %>%
          tidyr::unnest(data) %>%
          count(!!rlang::sym(nivel), wt= !!rlang::sym(var_pob), name="poblacion") %>%
          mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
          select(-!!rlang::sym(nivel))

        respuesta_fpc <- respuestas %>%
          count(!!rlang::sym(var_n)) %>%
          left_join(pob, by= var_n) %>%
          mutate(fpc_0=n/poblacion) %>%
          select(!!rlang::sym(var_n), fpc_0)

        muestra <- self$base %>%
          mutate(data = map(data,~.x %>% distinct(across(contains("fpc"))))) %>%
          tidyr::unnest(data) %>%
          select(-fpc_0) %>%
          mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
          inner_join(respuesta_fpc, by= var_n)

        self$base <- muestra
      },
      #' @description Construye el objeto tipo `design` usado por la paquetería [survey] para
      #'  alimentar el resto de la clase.
      #' @param respuestas [tibble()] que contiene las respuestas de los individuos entrevistados
      #' @param marco_muestral [tibble()] que contiene el marco muestral del proceso de muestreo
      #' @param tipo_encuesta Parametro heredado de la clase `Encuesta`. Define el tipo de diseno
      #'  muestral usado para calcular los pesos de los individuos
      #' @param sin_peso `LOGICAL`. Parámetro heredado de la clase encuesta. Si es `TRUE`, el diseno
      #'  se asume equiprobable.
      #' @param rake `LOGICAL` Parámetro heredado de la clase encuesta. Determina la postestratificacion
      #'  por edad y sexo.
      #' @param permitir_sin_conglomerados `LOGICAL`. Si el diseño con conglomerados
      #'  no puede construirse, el default (`FALSE`) detiene con un error informativo:
      #'  un diseño sin conglomerados ignora la correlación intraclase de las
      #'  entrevistas de una misma sección/manzana y subestima los errores
      #'  estándar (~25% medido en Chihuahua Ene-2026). Usa `TRUE` solo como
      #'  decisión consciente del analista; se emite un warning y el diseño se
      #'  construye estratificado sin conglomerados.
      #' @param umbral_rake Vector `c(inferior, superior)` con el rango aceptable
      #'  de los factores de ajuste del rake (default `c(0.5, 2)`). Sin cuotas en
      #'  campo, el rake absorbe TODA la corrección de composición demográfica:
      #'  factores fuera del umbral señalan un campo desbalanceado (revisar pases
      #'  de horario) y disparan la varianza de las estimaciones.
      extraer_diseno = function(respuestas, marco_muestral, tipo_encuesta, sin_peso, rake,
                                permitir_sin_conglomerados = FALSE,
                                umbral_rake = c(0.5, 2)){
        if(sin_peso){
          self$diseno <- survey::svydesign(
            ids=~1,
            data = respuestas
          )
        } else{
          r <- try(
            survey::svydesign(
              pps="brewer",
              ids=crear_formula_nombre(respuestas, "cluster_"),
              fpc = crear_formula_nombre(respuestas, "fpc_"),
              strata = crear_formula_nombre(respuestas, "strata_"),
              data = respuestas
            )
            ,T)

          diseno <- if("try-error" %in% class(r)){
            if(!permitir_sin_conglomerados){
              stop(
                "No se pudo construir el diseño muestral CON conglomerados (",
                trimws(conditionMessage(attr(r, "condition"))),
                "). Un diseño sin conglomerados (ids = ~1) ignora la ",
                "correlación intraclase por sección/manzana y subestima los ",
                "errores estándar de todo el estudio. Revisa las columnas ",
                "cluster_*/fpc_*/strata_* del snapshot (NAs en fpc suelen ",
                "venir de joins vacíos contra el plan muestral); si decides ",
                "degradar el diseño a propósito, usa ",
                "permitir_sin_conglomerados = TRUE.",
                call. = FALSE
              )
            }
            warning(
              "Diseño construido SIN conglomerados por decisión explícita ",
              "(permitir_sin_conglomerados = TRUE): los errores estándar ",
              "quedarán subestimados al ignorar la correlación intraclase ",
              "por sección/manzana.",
              call. = FALSE
            )
            out <- survey::svydesign(
              pps="brewer",
              ids = ~1,
              strata = crear_formula_nombre(respuestas, "strata_"),
              data = respuestas
            )
            out
          } else{
            r
          }

          if(rake){
            if(tipo_encuesta == "inegi"){
              pob <- marco_muestral %>%
                transmute(
                  P_18A24_F,
                  P_18A24_M,
                  P_25A59_F = P_18YMAS_F - P_18A24_F - P_60YMAS_F,
                  P_25A59_M = P_18YMAS_M - P_18A24_M - P_60YMAS_M,
                  P_60YMAS_F, P_60YMAS_M) %>%
                summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                pivot_longer(everything()) %>% mutate(name = gsub("P_","",name)) %>%
                separate(name, into = c("rango_edad", "sexo"))
            }

            if(tipo_encuesta == "ine"){
              pob <- marco_muestral %>%
                select(contains("LN22_")) %>%
                summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                separate(name, into = c("rango_edad", "sexo"))
            }


            pobG <- pob %>% count(rango_edad, wt = value, name = "Freq")
            pobS<- pob %>% count(sexo, wt = value, name = "Freq")
            self$diseno <- survey::rake(diseno, list(~rango_edad, ~sexo), list(pobG, pobS))

            # Monitoreo de los factores de ajuste del rake: sin cuotas en
            # campo, el rake carga toda la corrección de composición y su
            # costo es varianza (deff ~ 1 + CV^2 de los pesos). Se normaliza
            # por la mediana para aislar la COMPOSICIÓN: el reescalado
            # uniforme de nivel (suma de pesos -> total poblacional del
            # marco) no es señal de desbalance
            factor_rake <- as.numeric(stats::weights(self$diseno)) /
              as.numeric(stats::weights(diseno))
            factor_rake <- factor_rake / stats::median(factor_rake)
            message(sprintf(
              "Factores de ajuste del rake: min %.2f | mediana %.2f | max %.2f.",
              min(factor_rake), stats::median(factor_rake), max(factor_rake)
            ))
            if(min(factor_rake) < umbral_rake[1] || max(factor_rake) > umbral_rake[2]){
              warning(sprintf(
                paste0(
                  "Factores de rake fuera del umbral [%.2f, %.2f] ",
                  "(min %.2f, max %.2f): la composición del campo se desvía ",
                  "mucho de los márgenes poblacionales. Sin cuotas, esta es la ",
                  "señal para revisar los pases de horario/cobertura del ",
                  "levantamiento; los pesos extremos inflan la varianza de ",
                  "todas las estimaciones."
                ),
                umbral_rake[1], umbral_rake[2], min(factor_rake), max(factor_rake)
              ), call. = FALSE)
            }
          } else{
            self$diseno <- diseno
          }
        }
      },
      #' @description Visualiza las proporciones de sexo entre los individuos entrevistados en
      #'  campo y lo que el diseno muestral indica que debe ser entrevistado.
      revisar_sexo = function(){
        self$diseno$variables %>% count(sexo) %>%
          mutate(pct =n/sum(n), tipo = "encesta") %>% bind_rows(
            self$muestra$poblacion$marco_muestral %>%
              select(contains("LN22_")) %>%
              summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
              pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
              separate(name, into = c("rango_edad", "sexo")) %>%
              count(sexo, wt = value) %>%
              mutate(pct = n/sum(n), tipo = "real")
          ) %>% ggplot(aes(x = tipo, y = pct, color = sexo)) + geom_point() +
          ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
          geom_line(aes(group = sexo)) +
          scale_y_continuous(labels = scales::percent) +
          labs(y = NULL,  x = NULL) +
          theme_minimal()
      },
      #' @description Visualiza las proporciones de rangos de edad entre los individuos entrevistados
      #'  en campo y lo que el diseno muestral indica que debe ser entrevistado.
      revisar_rango_edad = function(){
        self$diseno$variables %>% count(rango_edad) %>%
          mutate(pct = n/sum(n), tipo = "encuesta") %>% bind_rows(
            self$muestra$poblacion$marco_muestral %>%
              select(contains("LN22_")) %>%
              summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
              pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
              separate(name, into = c("rango_edad", "sexo")) %>%
              count(rango_edad, wt = value )%>%
              mutate(pct = n/sum(n), tipo = "real")
          ) %>% ggplot(aes(x = tipo, y = pct, color = rango_edad)) + geom_point() +
          ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
          geom_line(aes(group = rango_edad)) +
          scale_y_continuous(labels = scales::percent) +
          labs(y = NULL,  x = NULL) +
          theme_minimal()
      },
      #' @description El método hace uso de la función [subset()] para tomar un subconjunto de los
      #'  datos obtenidos y producir resultados a partir de ellos respetando los pesos.
      #' @param seleccion Valor tipo caracter de la variable `región`.
      diseno_region = function(seleccion){
        self$region <- seleccion
        if(is.null(self$diseno_original)){
          self$diseno_original <- self$diseno
        }
        self$diseno <- subset(self$diseno_original, region %in% self$region)
      },
      #' @description Reinicia la modificación del diseño muestral aplicado a la clase. Si el diseño
      #'  muestral ha sido modificado ya sea por métodos de la clase `Muestra` o de alguna otra forma
      #'  este método garantiza aplica el diseño como si la clase recién se hubiera ejecutado.
      regresar_diseno_original = function(){
        self$region <- NULL
        self$calibracion <- NULL
        self$diseno_original -> self$diseno
      },
      #' @description Define calibraciones para aplicarlas al diseño muestral. El método está basado
      #'  en [survey::calibrate()]
      #' @param vars Vector que indica las variables usadas en la calibración
      #' @param poblacion poblacion poblacion contenida en cada estrato de la calibracion
      #' @param nombre Nombre asignado a la calibración. Sirve para identificar diferentes calibraciones
      agregar_calibracion = function(vars, poblacion, nombre){
        calibracion <- survey::calibrate(design = self$diseno,
                                         survey::make.formula(vars),
                                         population = poblacion)

        self$calibraciones <- self$calibraciones |>
          append(
            list(
              list(
                diseno = calibracion,
                variables = vars
              )
            ) |>
              purrr::set_names(nombre))
      },
      #' @description Calcula el total poblacional de una calibracion aplicada al diseño muestral.
      #' @param nombre Nombre asignado a la calibración. Sirve para identificar diferentes calibraciones
      revisar_calibracion = function(nombre){
        aux <- self$calibraciones |> purrr::pluck(nombre)
        survey::svytotal(survey::make.formula(aux |> pluck("variables")),
                         design = aux |> pluck("diseno"))
      },
      #' @description Compara las estimaciones para un mismo valor de diferentes variables aplciando
      #'  las calibraciones existentes al diseño muestral
      #' @param variables Vector tipo caracter que contiene los nombres de las variables a evaluar
      #' @param valor_variables Valores únicos de cada variable de interés para ser estimados
      #' @param vartype Argumento [vartype] de la función [srvyr::survey_mean()] que define el tipo
      #'  de calculo de la variación.
      comparar_calibraciones = function(variables, valor_variables, vartype){
        list(original = self$diseno) |>
          append(self$calibraciones |> purrr::map(~.x$diseno)) |>
          comparar_disenos(variables, valor_variables, vartype)
      },
      #' @description Aplica la calibración seleccionada al diseño muestra de la encuesta.
      #' @param nombre Nombre de la calibración a aplicar.
      elegir_calibracion = function(nombre){

        self$calibracion <- nombre

        if(is.null(self$diseno_original)){
          self$diseno_original <- self$diseno
        }

        self$diseno <- self$calibraciones |> purrr:::pluck(nombre, "diseno")
      }
    ))
