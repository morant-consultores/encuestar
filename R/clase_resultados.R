#' Clase Resultados
#'
#' @description La clase `Resultados` es el nodo principal entre el cálculo de las estimaciones y la
#'  interfaz de usuario. Después de la clase `Encuesta`, la clase `Resultados` es la segunda más
#'  importante dentro de la paquetería. Áunque tiene clases de jerarquía menor, estas clumplen el papel
#'  de clasificadoras de métodos. De esta forma, la clase `Resultados` escencialmente realiza dos pasos
#'  procesat y visualizar.
#'
#' @field encuesta Clase de jerarquía superior. Campo heredado por la clase `Encuesta`.
#' @field diseno Diseno muestral actual. Aunque el diseno esté contenido en la clase `Encuesta`,
#'  el modo de procesamiento de encuesta que no se levantó en campo requiere de alimentar la clase
#'  `Resultados` con un diseno muestral independiente a la clase `Muestra`.
#' @field diccionario Diccionario de procesamiento. Aunque el diccionario esté contendio en la
#'  clase `Cuestionario` que a su vez está dentro de la clase `Encuesta` el modo de procesamiento de
#'  encuesta que no se levantói en campo requiere de alimentar la clase con un diccionario que no
#'  haya sido procesado por la clase `Cuestionario`.
#' @field Descriptiva Clase de jerarquía menor. Contiene los métodos que visualizan resultados de
#'  forma más simple como gráficas de barras de resultados de una sola variable.
#' @field Regiones Clase de jerarquía menor, Contiene los métodos que visualizan resultados basados
#'  en la estratificación de la encuesta.
#' @field Modelo Clase de jerarquía menor. Contiene métodos de análisis estadísitico como PCA.
#' @field Cruce Clase de jerarquía menor. Contiene métodos enfocados en presentar resultados
#'  cruzando dos o más variables.
#' @field Especial Clase de jerarquía menor. Contiene métodos para visualizar resultados construidos
#'  de forma personalizada.
#' @field Tendencias Clase de jerarquía menor. Contiene los metodos de estimacions usando la media móvil
#' @field tema Objeto [theme] de la paquetería [ggplot2] que contiene el formato e identidad gráfica
#'  de Morant Consultores
#' @field graficadas En desuso. Contiene un [ tibble] que indica qué variables ya han sido procesadas
#'  durante la producción.
#'
#' @export
Resultados <-
  R6::R6Class(
    classname = "Resultados",
    public = list(
      encuesta = NULL,
      diseno = NULL,
      diccionario = NULL,
      Descriptiva = NULL,
      Regiones = NULL,
      Modelo = NULL,
      Cruce = NULL,
      Especial = NULL,
      Tendencias = NULL,
      NoRespuesta = NULL,
      tema = NULL,
      graficadas = NULL,
      #' @description Carga los insumos necesarios para producir resultados en
      #'  forma de visualizaciones con objetos tipo [ggplot2] o [flextable]
      #' @param encuesta Clase de jerarquía superior que se carga de forma
      #'  implítica.
      #' @param diseno Objeto tipo [desing] de la paquetería [survey]. Si el
      #'  parámetro es no nullo, el objeto `diseno` es el usado para calcular
      #'  las estimaciones. No se espera que ambos parámetros `encuesta` y
      #'  `diseno` sean no nulos al mismo tiempo.
      #' @param diccionario Diccionario de procesamiento de la encuesta.
      #' @param tema Objetio tipo [theme] de la paquetería [ggplot2].
      initialize = function(encuesta, diseno, diccionario, tema = tema_morant()){

        self$encuesta <- encuesta
        self$diseno <- diseno

        if(!is.null(self$encuesta)){

          self$diseno <- self$encuesta$muestra$diseno
          self$diccionario <- self$encuesta$cuestionario$diccionario

        } else {

          self$diseno <- diseno
          self$diccionario <- diccionario

        }

        self$tema <- tema

        self$Descriptiva <- Descriptiva$new(encuesta = self$encuesta,
                                            diseno = self$diseno,
                                            diccionario = self$diccionario,
                                            tema = self$tema,
                                            graficadas = self$graficadas)

        self$Cruce <- Cruce$new(encuesta = self$encuesta,
                                diseno = self$diseno,
                                diccionario = self$diccionario,
                                tema = self$tema,
                                graficadas = self$graficadas)

        self$Especial <- Especial$new(encuesta = self$encuesta,
                                      diseno = self$diseno,
                                      diccionario = self$diccionario,
                                      tema = self$tema,
                                      graficadas = self$graficadas)

        self$Tendencias <- Tendencias$new(encuesta = self$encuesta,
                                          diseno = self$diseno)

        self$NoRespuesta <- NoRespuesta$new(encuesta = self$encuesta,
                                            diseno = self$diseno,
                                            diccionario = self$diccionario,
                                            tema = self$tema)

        if(!is.null(self$encuesta)) {

          self$Regiones <- Regiones$new(encuesta = self$encuesta,
                                        diccionario = self$encuesta$cuestionario$diccionario,
                                        tema = self$tema)

          self$Modelo <- Modelo$new(diseno = self$encuesta$muestra$diseno,
                                    diccionario = self$encuesta$cuestionario$diccionario,
                                    tema = self$tema,
                                    graficadas = self$graficadas)

        } else {

          self$Regiones <- NULL
          self$Modelo <- Modelo$new(diseno = self$diseno,
                                    diccionario = self$diccionario,
                                    tema = self$tema,
                                    graficadas = self$graficadas)

        }
      },
      #' @description Visualiza las variables que aún no han sido producidas
      #'  por la paqutería durante la producción.
      faltantes = function(){
        if(!is.null(self$encuesta)) {
          gant_p_r(self$encuesta$cuestionario$diccionario %>% filter(!llaves %in% self$graficadas))
        } else {
          NULL
        }
      }
    )
  )

#' Clase Descriptiva
#'
#' @description La clase `Descriptiva` contiene los métodos necesarios para producir los resultados
#'  más básicos relacionados a la encuesta como graficas de barras de estimaciones de una sola variable
#'  o graficas tipo `gauge` que muestan el porcentaje de uno de los valores una variable binaria.
#' @field encuesta Clase de jerarquía superior. Se requiere para extraer el diseno de la encuesta.
#' @field diseno Objeto tipo [design] de la paquetería [survey] relacionado con el diseno de la
#'  encuesta.
#' @field diccionario Diccionario de procesamiento de la encuesta.
#' @field tema Objeto tipo [theme] de la paquetería [ggplot2]
#' @field graficadas Campo heredado por la clase `Resultados`.
Descriptiva <-
  R6::R6Class(
    classname = "Descriptiva",
    public = list(
      encuesta = NULL,
      diseno = NULL,
      diccionario = NULL,
      tema = NULL,
      graficadas = NULL,
      #' @description Define los insumos necesarios para preparar los métodos que producen los resultados
      #'  más básicos de la paquetería.
      #' @param encuesta Clase de jerarquía superior. Es el formato de la paquetería `encuestar`. Contiene
      #'  el diseno muestral de la encuesta.
      #' @param diseno Objeto tipo [design] de la paquetería [survey]. No se espera que los parametros
      #'  `encuesta` y `diseno` sean no nullos. El segundo sobreescribe el primero.
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      #' @param tema Objeto tipo [theme] de la paquetería [ggplot2].
      #' @param graficadas Parametro heredado de la clase `Resultados`.
      initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
        self$encuesta <- encuesta
        self$diseno <- diseno
        self$diccionario <- diccionario
        self$tema <- tema
        self$graficadas <- graficadas
      },
      #' @description Calcula las estimaciones de una variable y muestra los resultados en una
      #'  gráfica de barras ordenadas de mayor a menor de forma vertical de acuerdo a las esitmaciones.
      #' @param codigo Valor tipo caracter. Nombre de la variable que se va a analizar.
      #' @param salto Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a los valores únicos de la
      #'  variable de interés.
      #' @param porcentajes_fuera `LOGICAL`. Visualiza las labels que muestran las estimaciones dentro
      #'  o fuera de la barra. Útil para cuando hay muchas categorías.
      #' @param desplazar_porcentajes Valor real. Parámetro [nudge_y] de la función [geom_text]. Aplica
      #' sólo si `porcentajes_fuera` es `TRUE`.
      #' @param pct_otros Valor real definido entre 0 y 1. A las categorias cuyas estimaciones sean menor
      #'  o igual que `pct_otros` las agrupa, las suma y las muestra en `Otros`.
      #' @param orden_respuestas Vector tipo caracter que contiene, de manera ordenada, los valores únicos
      #'  de la variable de interés y reordena las barras de la gráfica de barras de acuerdo a ese orden.
      #'  @param text_size Valor tipo entero. Corresponde al tamaño de los porcentajes contenidos en la gráfica de barras.
      barras_categorica = function(codigo, salto = 20, porcentajes_fuera = F, desplazar_porcentajes = 0, pct_otros = 0.01, orden_respuestas = NA,text_size = 8){

        llave_aux <- codigo
        # if(!(llave_aux %in% self$graficadas)){
        #   if(llave_aux %in% self$diccionario$llaves){
        #     self$graficadas <- self$graficadas %>% append(llave_aux)
        #   } else {
        #     stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
        #   }
        # } else {
        #   warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
        # }

        if(!is.null(self$diccionario)) {

          tema <- self$diccionario |>
            filter(llaves == rlang::ensym(codigo)) |>
            pull(tema)

        }

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) %>%
          {
            if(!is.null(self$diccionario)) {
              mutate(.data = ., tema = tema)
            } else {
              .
            }
          } %>%
          mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = pct_otros, w = media, other_level = "Otros"),
                 respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
          group_by(respuesta) |>
          summarise(media = sum(media)) |>
          graficar_barras(salto = salto,
                          porcentajes_fuera = porcentajes_fuera,
                          desplazar_porcentajes = desplazar_porcentajes,
                          orden_respuestas = orden_respuestas,
                          text_size =text_size ) +
          self$tema

      },
      #' @description Calcula las estimaciones de varias variables cuyos nombres inician con el mismo
      #'  patron de caracteres. Las variables tienen que contener al menos un valor común entre todas.
      #'  El formato esperado de los nombres de las variables son patron_inicial_aspecto1, patron_inicial_aspecto2
      #'  patron_inicial_aspecto3 etc.
      #' @param patron_inicial Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables.
      #' @param aspectos Vector tipo caracter que identifica a las diferentes variables a procesar. De
      #'  acuerdo al método, el ejemplo sería c("aspecto1", "aspecto2", "aspecto3").
      #' @param salto Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje x.
      #' @param filtro Valor tipo caracter. Operación de identidad que se aplica como filtro entre los
      #'  resultados de las variables de interés. Por defecto, la variable que contiene los valores
      #'  únicos se llama "respuesta" por lo que el filtro aplica sobre esa variable muda. por ejemplo
      #'  filtro = "respuesta == 'azul'".
      #' @param porcentajes_fuera `LOGICAL`. Visualiza las labels que muestran las estimaciones dentro
      #'  o fuera de la barra. Útil para cuando hay muchas categorías.
      #' @param desplazar_porcentajes Valor real. Parámetro [nudge_y] de la función [geom_text]. Aplica
      #' sólo si `porcentajes_fuera` es `TRUE`.
      #' @param text_size Valor tipo entero. Corresponde al tamaño de los porcentajes contenidos en la gráfica de barras.
      barras_aspectos = function(patron_inicial, aspectos = NULL, salto = 20, filtro = "respuesta == 'Sí'", porcentajes_fuera = F, desplazar_porcentajes = 0,text_size=8){

        if(is.null(filtro) | is.null(aspectos)) {

          stop(paste("Especifique la respuesta en la cual hacer filtro con el argumento `filtro` o indique los aspectos correctos."))

        } else {

          tema <- self$diccionario |>
            filter(llaves == rlang::ensym(patron_inicial)) |>
            pull(pregunta)

          if(is.null(self$diseno)) {

            diseno <- self$encuesta$muestra$diseno

          } else {

            diseno <- self$diseno

          }

          analizar_frecuencias_aspectos(diseno = diseno,
                                        diccionario = self$diccionario,
                                        patron_pregunta = {{patron_inicial}},
                                        aspectos = aspectos) |>
            left_join(self$diccionario |>
                        select(llaves, tema), by = c("aspecto" = "llaves")) |>
            filter(eval(rlang::parse_expr(filtro))) |>
            transmute(respuesta = tema, media) |>
            graficar_barras(salto = salto,
                            porcentajes_fuera = porcentajes_fuera,
                            desplazar_porcentajes = desplazar_porcentajes,
                            orden_respuestas = NA,
                            text_size=text_size) +
            self$tema

        }

      },
      #' @description Calcula las estimaciones de varias variables cuyos nombres inician con el mismo
      #'  patrón de caracteres. Dichas variables representan un conjunto de preguntar multirespuesta.
      #'  Muestra los resultados en un gráfico tipo gráfica de barras.
      #' @param patron_inicial Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables.
      #' @param salto Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje x.
      #' @param porcentajes_fuera `LOGICAL`. Visualiza las labels que muestran las estimaciones dentro
      #'  o fuera de la barra. Útil para cuando hay muchas categorías.
      #' @param desplazar_porcentajes Valor real. Parámetro [nudge_y] de la función [geom_text]. Aplica
      #' sólo si `porcentajes_fuera` es `TRUE`.
      #' @param text_size Valor tipo entero. Corresponde al tamaño de los porcentajes contenidos en la gráfica de barras.
      barras_multirespuesta = function(patron_inicial, salto = 20, porcentajes_fuera = F, desplazar_porcentajes = 0,text_size=8){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_frecuencias_multirespuesta(diseno = diseno,
                                            patron_inicial) %>%
          graficar_barras(salto = salto,
                          porcentajes_fuera = porcentajes_fuera,
                          desplazar_porcentajes = desplazar_porcentajes,
                          text_size=text_size) +
          self$tema

      },
      #' @description Calcula las estimaciones de una única variable que representa una pregunta
      #'  cuyas respuestas son únicamente del tipo numérica.
      #' @param codigo Valor tipo caracter. Nombre de la variable que se va a analizar.
      #' @param color Valor tipo caracter. Color en formato `HEX`. Es ekl color con el que se 'rellena'
      #'  el gráfico tipo gauge.
      #' @param escala Dupla tipo numérica. Son los posibles valores mínimos y máximos posibles asociados
      #'  a la pregunta que representa la variable.
      #' @param size_text_pct Valor entero. Determina el tamano del dentro dento del gráfico. Es el parámetro
      #' [size] de la función [ggplot2::geom_text()].
      gauge_numerica = function(codigo, color = "#850D2D", escala = c(0, 10), size_text_pct = 14){

        llave_aux <- codigo
        if(!(llave_aux %in% self$graficadas)){
          if(llave_aux %in% self$diccionario$llaves){
            self$graficadas <- self$graficadas %>% append(llave_aux)
          } else {
            stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
          }
        } else {
          warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
        }

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        bd_estimacion <- analizar_frecuencias(diseno = diseno, pregunta = {{codigo}})

        tema <- self$diccionario |>
          filter(llaves == rlang::ensym(codigo)) |>
          pull(tema)

        bd_estimacion |>
          mutate(tema = tema) |>
          graficar_gauge(color_principal = color,
                         escala = escala,
                         size_text_pct = size_text_pct)

      },
      #' @description Calcula las estimaciones de un valor único de una única variable y lo muestra en
      #'  una gráfica tipo gauge.
      #' @param codigo Valor tipo caracter. Nombre de la variable que se va a analizar.
      #' @param filtro Valor tipo caracter. Operación de identidad que se aplica como filtro entre los
      #'  resultados de la variable de interés. Por ejemplo filtro = "respuesta == 'azul'".
      #' @param color Valor tipo caracter. Color en formato `HEX`. Es el color con el que se 'rellena'
      #'  el gráfico tipo gauge.
      #' @param escala Dupla tipo numérica. Por lo general, la escala es porcentual.
      #' @param size_text_pct Valor entero. Determina el tamano del dentro dento del gráfico. Es el parámetro
      #'  [size] de la función [ggplot2::geom_text()].
      gauge_categorica = function(codigo, filtro, color = "#850D2D", escala = c(0, 1), size_text_pct = 14){

        if(is.null(filtro)) {

          stop(paste("Especifique la respuesta en la cual hacer filtro con el argumento `filtro`"))

        }

        else {

          llave_aux <- codigo
          if(!(llave_aux %in% self$graficadas)){
            if(llave_aux %in% self$diccionario$llaves){
              self$graficadas <- self$graficadas %>% append(llave_aux)
            } else {
              stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
            }
          } else {
            warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
          }

          if(is.null(self$diseno)) {

            diseno <- self$encuesta$muestra$diseno

          } else {

            diseno <- self$diseno

          }

          bd_estimacion <- analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) |>
            filter(eval(rlang::parse_expr(filtro)))

          tema <- self$diccionario |>
            filter(llaves == rlang::ensym(codigo)) |>
            pull(tema)

          bd_estimacion %>%
            mutate(tema = tema) |>
            graficar_gauge(color_principal = color,
                           escala = escala,
                           size_text_pct = size_text_pct)
        }

      },
      #' @description Calcula las estimaciones de varias variables cuyos nombres inician con el mismo
      #'  patrón de caractere, cuyas respuestas son del tipo numérico y lo muestra en una gráfica única
      #'  usando un [ggplot2::geom_pointrange] para cada variable. Por ejemplo calificacion_nombre1,
      #'  calificacion_nombre2, calificacion_nombre3
      #' @param patron Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables. De acuerdo al método sería "calificacion"
      #' @param aspectos Vector tipo caracter que identifica a las diferentes variables a procesar. De
      #'  acuerdo al ejemplo del método sería c("nombre1", "nombre2", "nombre3").
      #' @param escala Dupla tipo numérica. Son los posibles valores mínimos y máximos posibles asociados
      #'  a la pregunta que representa la variable.
      #' @param point_size valor tipo entero. Determina el tamano del punto en la visuaización. Es
      #'  el parámetro [size] de la funcion [ggplot2::geom_pointrange()]
      #' @param text_point_size valor tipo entero. Determina el tamano del texto mostrado arriba del
      #'  punto. Es el parámetro [size] de la funcion [ggplot2::geom_text()]
      intervalo_numerica = function(patron, aspectos, escala = c(0, 10), point_size = 1, text_point_size = 8){

        # llave_aux <- codigo
        # if(!(llave_aux %in% self$graficadas)){
        #   if(llave_aux %in% self$diccionario$llaves){
        #     self$graficadas <- self$graficadas %>% append(llave_aux)
        #   } else {
        #     stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
        #   }
        # } else {
        #   warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
        # }

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_frecuencias_aspectos(diseno = diseno,
                                      diccionario = self$diccionario,
                                      patron_pregunta = {{patron}},
                                      aspectos = aspectos) %>%
          left_join(self$diccionario %>%
                      select(aspecto = llaves, tema), by = "aspecto") |>
          graficar_intervalo_numerica(escala = escala, point_size = point_size, text_point_size = text_point_size) +
          self$tema


      },
      #' @description Calcula las estimaciones de una variable y muestra los resultados en una
      #'  gráfica tipo lollipops ordenadas de mayor a menor de forma vertical de acuerdo a las esitmaciones.
      #' @param codigo Valor tipo caracter. Nombre de la variable que se va a analizar.
      #' @param orden Vector tipo caracter que contiene, de manera ordenada, los valores únicos de
      #'  la variable de interés y reordena las barras de la gráfica de barras de acuerdo a ese orden.
      #' @param limits Dupla tipo numérica. Delimita el límite inferior y superior del gráfico. Es el
      #'  parámetro [limits] de la función [ggplot2::scale_y_continuous()].
      #' @param width_cats Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje x.
      #' @param size Valor tipo entero. define el grueso de la linea del gráfico tipo lollipop. Es el
      #'  argumento [linewidth] de la función [ggplot2::geom_segment()]
      #' @param size_pct Valor entero. Determina el tamano del dentro dento del gráfico. Es el parámetro
      #'  [size] de la función [ggplot2::geom_text()].
      #' @param pct_otros Valor real definido entre 0 y 1. A las categorias cuyas estimaciones sean menor
      #'  o igual que `pct_otros` las agrupa, las suma y las muestra en `Otros`.
      lollipops_categorica = function(codigo, orden = NULL, limits = c(0, 1.0), width_cats = 15 , size = 3, size_pct = 6, pct_otros = 0.01){
        llave_aux <- codigo
        if(!(llave_aux %in% self$graficadas)){
          if(llave_aux %in% self$diccionario$llaves){
            self$graficadas <- self$graficadas %>% append(llave_aux)
          } else {
            stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
          }
        } else {
          warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
        }

        tema <- self$diccionario |>
          filter(llaves == rlang::ensym(codigo)) |>
          pull(tema)

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) |>
          mutate(tema = tema) |>
          rename(pct = media) |>
          mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = pct_otros, w = pct, other_level = "Otros"),
                 respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
          group_by(respuesta) |>
          summarise(pct = sum(pct)) |>
          graficar_lollipops(orden = orden,
                             limits = limits,
                             width_cats = width_cats ,
                             size = size,
                             size_pct = size_pct) +
          self$tema
      },
      #' @description Calcula las estimaciones de varias variables cuyos nombres inician con el mismo
      #'  patrón de caracteres. Dichas variables representan un conjunto de preguntar multirespuesta.
      #'  Muestra los resultados en un gráfico tipo lollipops.
      #' @param patron_inicial Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables.
      #' @param orden Vector tipo caracter que contiene, de manera ordenada, los valores únicos
      #'  de la variable de interés y reordena las barras de la gráfica de barras de acuerdo a ese orden.
      #' @param limits Dupla tipo numérica. Son los posibles valores mínimos y máximos posibles asociados
      #'  a la pregunta que representa la variable.
      #' @param width_cats Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje x.
      #' @param size Valor tipo entero. define el grueso de la linea del gráfico tipo lollipop. Es el
      #'  argumento [linewidth] de la función [ggplot2::geom_segment()]
      #' @param size_pct Valor entero. Determina el tamano del dentro dento del gráfico. Es el parámetro
      #'  [size] de la función [ggplot2::geom_text()].
      lollipops_multirespuesta = function(patron_inicial, orden = NULL, limits = c(0, 1.0), width_cats = 15 , size = 3, size_pct = 6){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_frecuencias_multirespuesta(diseno = diseno,
                                            patron_inicial) %>%
          rename(pct = media) |>
          graficar_lollipops(orden = orden,
                             limits = limits,
                             width_cats = width_cats ,
                             size = size,
                             size_pct = size_pct)  +
          self$tema

      }
    ))

#' Clase Cruce
#'
#' @description La clase `Cruce` contiene los métodos necesarios para producir los resultados
#'  relacionados de crucar dos o más variables.
#' @field encuesta Clase de jerarquía superior. Se requiere para extraer el diseno de la encuesta.
#' @field diseno Objeto tipo [design] de la paquetería [survey] relacionado con el diseno de la
#'  encuesta.
#' @field diccionario Diccionario de procesamiento de la encuesta.
#' @field tema Objeto tipo [theme] de la paquetería [ggplot2]
#' @field graficadas Campo heredado por la clase `Resultados`.
Cruce <-
  R6::R6Class(
    classname = "Cruce",
    public = list(
      encuesta = NULL,
      diseno = NULL,
      diccionario = NULL,
      tema = NULL,
      graficadas = NULL,
      #' @description Define los insumos necesarios para preparar los métodos que producen los resultados
      #'  entre dos o más variables.
      #' @param encuesta Clase de jerarquía superior. Es el formato de la paquetería `encuestar`. Contiene
      #'  el diseno muestral de la encuesta.
      #' @param diseno Objeto tipo [design] de la paquetería [survey]. No se espera que los parametros
      #'  `encuesta` y `diseno` sean no nullos. El segundo sobreescribe el primero.
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      #' @param tema Objeto tipo [theme] de la paquetería [ggplot2].
      #' @param graficadas Parametro heredado de la clase `Resultados`.
      initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
        self$encuesta <- encuesta
        self$diseno <- diseno
        self$diccionario <- diccionario
        self$tema <- tema
        self$graficadas <- graficadas
      },
      #' @description Calcula el cruce entre dos variables donde una de las dos es binaria. Visualiza
      #'  los resultados usando [treemapify::geom_treemap()]. El método también funciona para casos
      #'  en que la variable principal no es binaria pero no se recomienda su uso.
      #' @param variable_principal Valor tipo caracter que indica la variable principal que idealmente
      #'  es binaria.
      #' @param variable_secundaria Valor tipo caracter que indica la variable secundaria. No se recomienda
      #'  varaibles tipo numérico.
      #' @param colores_variable_secundaria Vector que identifica los colores asociados a los valores
      #'  únicos de la variable secundaria. Es el argumento [values] de la función [ggplot2::scale_fill_manual()].
      #' @param filter_variable_secundaria Valor tipo caracter. Operación de identidad que se aplica como filtro entre los
      #'  resultados de la variable secundaria.
      #' @param vartype Parametro de [srvyr::survey_mean()] que reporta la variabilidad de la estimacion
      #' @param linea_grosor Valor tipo entero. Determina el grosor de la linea que divide a los
      #'  'subbloques' dentro de cada grupo.
      #' @param linea_color Valor tipo caracter. Color en formato `HEX`. Es el color de la linea que
      #'  separa a los 'subbloques'
      #' @param na_rm `LOGICAL`. En el cálculo de estimaciones, se omiten los valores nulos.
      bloques = function(variable_principal,
                         variable_secundaria,
                         colores_variable_secundaria,
                         filter_variable_secundaria = NULL,
                         vartype = "cv",
                         linea_grosor = 2,
                         linea_color = "white",
                         na_rm = TRUE){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_cruce(diseno = diseno,
                       variable_principal = variable_principal,
                       variable_secundaria = variable_secundaria,
                       vartype = vartype,
                       na_rm = na_rm) %>%
          {
            if(!is.null(filter_variable_secundaria)){
              filter(.data = ., !!rlang::sym(variable_secundaria) %in% filter_variable_secundaria)
            } else {
              .
            }
          } %>%
          graficar_cruce_bloques(cruce = variable_principal,
                                 variable = variable_secundaria,
                                 colores_variable_secundaria = colores_variable_secundaria,
                                 vartype = vartype,
                                 linea_grosor = linea_grosor,
                                 linea_color = linea_color) +
          self$tema
      },
      #' @description Calcula el cruce entre dos variables no necesariamente binarias y muestra el
      #'  resultado usando [ggplot2::geom_line()] para cada valor de la variable secundaria.
      #' @param variable_principal Valor tipo caracter que indica la variable principal. Las categorías
      #'  se muestran en el eje x.
      #' @param variable_secundaria Valor tipo caracter que indica la variable secundaria.
      #' @param orden_variable_principal Vector tipo caracter. Contiene los valores únicos de la
      #'  variable principal en forma ordenada.
      #' @param valores_variable_secundaria Vector tipo caracter  que contiene los valores de interés
      #'  de la variable secundaria.
      #' @param colores_variable_secundaria Vector que identifica los colores asociados a los valores
      #'  únicos de la variable secundaria. Es el argumento [values] de la función [ggplot2::scale_fill_manual()].
      #' @param limits Dupla tipo numérica. Son los posibles valores mínimos y máximos posibles asociados
      #'  a la pregunta que representa la variable.
      #' @param wrap_x Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje x.
      #' @param wrap_legend Valor tipo entero. Reduce o aumenta la longitud de las `legends` del gráfico
      #' @param text_nudge_y Valor real. Parámetro [nudge_y] de la función [geom_text] aplicado a la
      #'  función [ggrepel::geom_text_repel()].
      #' @param size_text Valor entero. Determina el tamano del dentro del gráfico. Es el parámetro
      #' [size] de la función [ggrepel::geom_text_repel()].
      #' @param size_text_x Valor entero. Determina el tamano del dentro del eje x. Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.x]
      #' @param size_text_y Valor entero. Determina el tamano del dentro del eje y. Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.y]
      #' @param size_text_legend Valor entero. Determina el tamano del dentro de las legends Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.legend]
      #' @param na_rm `LOGICAL`. En el cálculo de estimaciones, se omiten los valores nulos.
      lineas = function(variable_principal,
                        variable_secundaria,
                        orden_variable_principal,
                        valores_variable_secundaria = NULL,
                        colores_variable_secundaria,
                        limits =  c(0, 0.75),
                        wrap_x = 25,
                        wrap_legend = 25,
                        text_nudge_y = 0.01,
                        size_text = 8,
                        size_text_x = 16,
                        size_text_y = 14,
                        size_text_legend = 14,
                        na_rm = TRUE){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_cruce(diseno = diseno,
                       variable_principal = variable_principal,
                       variable_secundaria = variable_secundaria,
                       vartype = "cv",
                       na_rm = na_rm) %>%
          {
            if(!is.null(valores_variable_secundaria)) {
              filter(., !!rlang::sym(variable_secundaria) %in% valores_variable_secundaria)
            } else {
              .
            }
          } %>%
          rename(var_x = !!rlang::sym(variable_principal),
                 var_y = !!rlang::sym(variable_secundaria),
                 media = coef) |>
          graficar_lineas(orden_var_x = orden_variable_principal,
                          colores = colores_variable_secundaria,
                          salto_x = wrap_x,
                          salto_legend = wrap_legend,
                          limits = limits,
                          text_nudge_y = text_nudge_y,
                          size_text = size_text) +
          self$tema +
          theme(legend.position = "bottom",
                axis.text.x = element_text(size = size_text_x),
                axis.text.y = element_text(size = size_text_y),
                legend.text = element_text(size = size_text_legend))
      },
      #' @description Calcula el cruce entre dos variables categoricas donde la primera o principal
      #'  es binaria, filtra los valores de interés de la segunda variable y muestra las diferencias
      #'  relativas a la primera en una gráfica tipo lolipop.
      #' @param variable_principal Valor tipo caracter que indica la variable principal. Las categorías
      #'  se muestran en el eje y.
      #' @param variables_secundarias Valor tipo caracter que indica las variables secundarias.
      #' @param filtro_variables_secundarias Valor tipo caracter. Valor en común entre las variables
      #'  secundarias y que es de interés para mostrar resultados.
      #' @param orden_variablePrincipal Vector tipo caracter. Contiene los valores únicos de la
      #'  variable principal en forma ordenada.
      #' @param colores_variables_secundarias Vector que identifica los colores asociados a los valores
      #'  únicos de las variables secundaria. Es el argumento [values] de la función [ggplot2::scale_fill_manual()].
      #' @param caption Valor tipo caracter. Es el [caption] del gráfico.
      #' @param nudge_x Valor real. Parámetro [nudge_x] de la función [ggplot2::geom_text()].
      #' @param size_geom_text Valor entero. Determina el tamano del dentro del gráfico. Es el parámetro
      #'  [size] de la función [ggplot2::geom_text()].
      #' @param invertir_variables `LOGICAL`. Determina si las variables principal y secundarias se
      #'  invierten en el gráfico.
      #' @param vartype Parametro de [srvyr::survey_mean()] que reporta la variabilidad de la estimacion
      #' @param limits Dupla tipo numérica. Son los posibles valores mínimos y máximos posibles asociados
      #'  a la pregunta que representa la variable.
      #' @param wrap_y Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje y.
      #'  Aplica la función [str_wrap()] de la paquetería [stringr] a las labels del eje y.
      #' @param wrap_caption Valor tipo entero. Reduce o aumenta la longitud del texto mostrado en
      #'  el caption.
      #' @param size_text_x Valor entero. Determina el tamano del dentro del eje x. Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.x]
      #' @param size_text_y Valor entero. Determina el tamano del dentro del eje y. Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.y]
      #' @param size_text_caption Valor entero. Determina el tamano del dentro de las legends Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.caption]
      #' @param size_text_legend Valor entero. Determina el tamano del dentro de las legends Es el parámetro
      #'  [size] de la función [ggplot2::element_text()] aplicado a [axis.text.legend]
      #' @param ver_diferencias `LOGICAL`. Determina si se muestran las diferencias en puntos porcentuales
      #'  dentro del gráfico.
      #' @param traslape Valor bopoleano. En caso de ser TRUE, realiza un factor de ajuste para los porcentajes presentados por [ggplot2::geom_text()].
      #' Solo afecta a los valores cuya diferencia máxima y mínima se menor que el factor de tolerancia [limite_dif_pct].  Por defecto tiene valor FALSE.
      #' @param limite_dif_pct Valor de tipo decimal. Es el factor o nivel de tolerencia para la diferencia entre dos puntos
      #' que al graficar sus porcentajes tienen un nivel de traslape. Solo se efectua si [traslape] es TRUE. Por defecto su valor es 0.02.
      #' @param ajuste_pos Valor de tipo decimal. Es el factor de ajuste de posición que los porcentajes
      #' graficados tomarán en caso de traslaparse. El porcentaje menor se despalzará la izquierda, y el mayor a la derecha. Solo se efectua si [traslape] es TRUE. Por defecto su valor es 0.02.
      #'
      #'
      lolipop_diferencias = function(variable_principal,
                                     variables_secundarias,
                                     filtro_variables_secundarias,
                                     orden_variablePrincipal,
                                     colores_variables_secundarias,
                                     caption,
                                     nudge_x = 0.05,
                                     size_geom_text = 6,
                                     invertir_variables = F,
                                     vartype = "cv",
                                     limits = c(0, 0.75),
                                     wrap_y = 25,
                                     wrap_caption = 25,
                                     size_text_x = 16,
                                     size_text_y = 16,
                                     size_text_caption = 16,
                                     size_text_legend = 16,
                                     ver_diferencias = TRUE,
                                     traslape = F,
                                     limite_dif_pct = 0.02,
                                     ajuste_pos = 0.02) {
        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        bd_estimacion <-
          analizar_cruce_aspectos(diseno = diseno,
                                  variable_principal = variable_principal,
                                  variables_secundarias = variables_secundarias,
                                  filtro_variables_secundarias = filtro_variables_secundarias,
                                  vartype = vartype) |>
          left_join(self$diccionario |>
                      distinct(llaves, tema), by = c("variable" = "llaves")) |>
          select(!variable) |>
          rename("variable_principal" := variable_principal)

        bd_estimacion <-
          if(invertir_variables) {
            bd_estimacion |>
              transmute(aux = variable_principal,
                        variable_principal = tema,
                        tema = aux,
                        mean,
                        cv) |>
              select(!aux)
          } else {
            bd_estimacion
          }

        bd_estimacion <-
          if(ver_diferencias) {
            bd_estimacion |>
              group_by(variable_principal)|>
              mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
                     mean_dif = (max(mean)-min(mean)))|>
              ungroup()
          } else {
            bd_estimacion
          }

        bd_estimacion |>
          graficar_lolipop_diferencias(orden_variablePrincipal = orden_variablePrincipal,
                                       colores_variables_secundarias = colores_variables_secundarias,
                                       nudge_x = nudge_x,
                                       size_geom_text = size_geom_text,
                                       caption = caption,
                                       wrap_y = wrap_y,
                                       wrap_caption = wrap_caption,
                                       limits = limits,
                                       traslape = traslape,
                                       limite_dif_pct = limite_dif_pct,
                                       ajuste_pos = ajuste_pos) +
          {if(ver_diferencias)  geom_text(aes(label = scales::percent(x = mean_dif , accuracy = 1.),y = mean_diff_pos, colour = 'gray' ),
                                          nudge_x = -nudge_x, size = size_geom_text, show.legend = F) }+
          self$tema +
          theme(legend.position = "bottom",
                axis.text.x = element_text(size = size_text_x),
                axis.text.y = element_text(size = size_text_y),
                legend.text = element_text(size = size_text_legend),
                plot.caption = element_text(size = size_text_caption))

      },
      #' @description Muestra las proporciones de individuos correspondientes a valores unicos entre
      #'  dos variables en una gráfica tipo `sankey`
      #' @param variables Vector tipo caracter que contiene las dos variables a visualizar.
      #' @param colores Vector que identifica los colores asociados a los valores únicos de las variables
      #'  Es el argumento [values] de la función [ggplot2::scale_fill_manual()] y [ggplot2::scale_color_manual()]
      #' @param size_text_cat Parametro [size] de la funcion [ggsankey::geom_sankey_text()] que determina
      #'  el tamano del texto de las etiquetas en cada nodo del gráfico.
      #' @param width_text Valor tipo entero. Reduce o aumenta la longitud del texto mostrado en las etiquetas
      #' @param omitir_valores_variable1 Vector tipo caracter. Valores únicos de la primer variable
      #'  que se omiten de los resultados.
      #' @param omitir_valores_variable2 Vector tipo caracter. Valores únicos de la segunda variable
      #'  que se omiten de los resultados.
      sankey_categorica = function(variables = NULL,
                                   colores,
                                   size_text_cat = 6,
                                   width_text = 15,
                                   omitir_valores_variable1 = NULL,
                                   omitir_valores_variable2 = NULL){

        if(is.null(variables)) {

          stop(paste("Especifique las variables a analizar con el argumento `variables`"))

        } else {

          if(is.null(self$diseno)) {

            diseno <- self$encuesta$muestra$diseno

          } else {

            diseno <- self$diseno

          }

          bd_estimacion <- analizar_sankey(diseno = diseno,
                                           variables = variables,
                                           filtro_var1 = omitir_valores_variable1,
                                           filtro_var2 = omitir_valores_variable2)

          graficar_sankey(bd = bd_estimacion,
                          variables = variables,
                          colores = colores,
                          size_text_cat = size_text_cat,
                          width_text = width_text)
        }
      },
      #' @description Muestra las proporciones de individuos correspondientes a valores unicos entre
      #'  dos variables en una tabla renderizada mediante la paquetería [flextable].
      #' @param var1 Valor tipo caracter. Es el nombre de la variable principal del cruce.
      #' @param var2 Valor tipo caracter. Nombre de la variable secundaria del cruce.
      #' @param filtro_var2 Vector tiopo caracter. Son los valores únicos de la segunda variable que
      #'  se omiten de los resultados.
      #' @param etiquetas Dupla ordenada tipo caracter. Nombres de las variables para mostrar en el
      #'  resultado final.
      #' @param colores_var1 Vector que identifica los colores asociados a los valores únicos de la
      #'  primer variable.
      #' @param colores_var2 Vector que identifica los colores asociados a los valores únicos de la
      #'  segunda variable.
      #' @param size_text_header Valor tipo entero. Determina el tamano del texto en el encabezado
      #'  de la tabla.
      #' @param size_text_body Valor tipo entero. Determina el tamano del texto en el cuerpo de la
      #'  tabla.
      #' @param salto Valor tipo entero. Reduce o aumenta la longitud del texto mostrado en
      #'  el caption.
      #' @param na_rm `LOGICAL`. En el cálculo de estimaciones, se omiten los valores nulos.
      tabla_votoCruzado = function(var1,
                                   var2,
                                   filtro_var2 = NULL,
                                   etiquetas = c("variable 1", "variable 2"),
                                   colores_var1,
                                   colores_var2,
                                   size_text_header = 18,
                                   size_text_body = 14,
                                   salto = 20,
                                   na_rm = TRUE){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        # if(is.null(filtro_var2)) {
        #   filtro_var2 <-
        #     diseno$variables |>
        #     as_tibble() |>
        #     distinct(!!rlang::sym(var2)) |>
        #     pull()
        # }

        bd_votoCruzado <-
          calcular_tabla_votoCruzado(diseno = diseno,
                                     var1 = var1,
                                     var2 = var2,
                                     filtro_var2 = filtro_var2,
                                     na_rm = na_rm)

        tabla_salida <-
          formatear_tabla_votoCruzado(tabla_votoCruzado = bd_votoCruzado,
                                      var1 = var1,
                                      var2 = var2,
                                      filtro_var2 = filtro_var2,
                                      etiquetas = etiquetas,
                                      colores_var1 = colores_var1,
                                      colores_var2 = colores_var2,
                                      size_text_header = size_text_header,
                                      size_text_body = size_text_body,
                                      salto = salto)

        return(tabla_salida)
      }
    ))

#' Clase Especial
#'
#' @description La clase `Especial` contiene los métodos necesarios para producir los resultados
#'  cuya naturaleza inherente de la pregunta requiera de una adaptación particular o compuesta
#'  para visualizar los resultados.
#' @field encuesta Clase de jerarquía superior. Se requiere para extraer el diseno de la encuesta.
#' @field diseno Objeto tipo [design] de la paquetería [survey] relacionado con el diseno de la
#'  encuesta.
#' @field diccionario Diccionario de procesamiento de la encuesta.
#' @field tema Objeto tipo [theme] de la paquetería [ggplot2]
#' @field graficadas Campo heredado por la clase `Resultados`.
Especial <-
  R6::R6Class(
    classname = "Especial",
    public = list(
      encuesta = NULL,
      diseno = NULL,
      diccionario = NULL,
      tema = NULL,
      graficadas = NULL,
      #' @description Define los insumos necesarios para preparar los métodos que producen los resultados
      #'  que requieren un tipo de visualización particular o compuesta.
      #' @param encuesta Clase de jerarquía superior. Es el formato de la paquetería `encuestar`. Contiene
      #'  el diseno muestral de la encuesta.
      #' @param diseno Objeto tipo [design] de la paquetería [survey]. No se espera que los parametros
      #'  `encuesta` y `diseno` sean no nullos. El segundo sobreescribe el primero.
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      #' @param tema Objeto tipo [theme] de la paquetería [ggplot2].
      #' @param graficadas Parametro heredado de la clase `Resultados`.
      initialize = function(encuesta = NULL,
                            diseno = NULL,
                            diccionario = NULL,
                            tema,
                            graficadas = NULL){
        self$encuesta <- encuesta
        self$diseno <- diseno
        self$diccionario <- diccionario
        self$tema <- tema
        self$graficadas <- graficadas
      },
      #' @description Visualiza los resultados de preguntas con mismas posibles respuestas y aplicadas
      #'  sobre diferentes tópicos. Usualmente este tipo de preguntas son asociadas a la opinión y/o
      #'  el conocimiento de personajes. El conjunto de preguntas están identificas, por ejemplo, como
      #'  conocimiento_persona1, conocimiento_persona2, etc y opinion_persona1, opinion_persona2, etc.
      #'  Por lo general, las variables de opinion destán condicionadas por las de conocimiento. Esto
      #'  está considerado en el procesamiento de los resultados.
      #' @param patron_inicial Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables que representan la opinion. En el ejemplo este parámetro seía
      #'  "opinion".
      #' @param aspectos Vector tipo caracter que identifica a las diferentes variables a procesar. De
      #'  acuerdo al método, el ejemplo sería c("persona1", "persona2").
      #' @param ns_nc Valor tipo caracter. Es el valor asociado a la respuesta  'No sabe o no contesta'
      #'  en la pregunta de opinión. Ya que este valor no aporta información, se separa de los resultados.
      #' @param regular Valor tipo caracter. Es el valor asociado a la respuesta  'Regular' que
      #'  representa una opinión neutra.
      #' @param llave_burbuja Valor tipo caracter. Si es no vacío, se visualizan las estimaciones
      #'  relacionadas a la pregunta de conocimiento del personaje.
      #' @param filtro_burbuja Valor tipo caracter. Operación de identidad que se aplica como filtro
      #'  entre los resultados de las preguntas asociadas al conocimiento. Por lo general el filtro
      #'  tiene la forma filtro = "respuesta == 'Sí'". Este parámetro no afecta al cálculo de las
      #'  estimaciones de las variables asociadas a la opinion, unicamente a las asociadas al conocimiento.
      #' @param size_burbuja Valor tipo entero. Determina el tamano de las "burbujas" que se usan para
      #'  visualizar los resultados de las preguntas asociadas al conocimiento. Es el parámetro [max_size]
      #'  de la función [ggplot2::scale_size_area()].
      #' @param grupo_positivo Vector tipo caracter. Indica los valores únicos de las variables asociadas
      #'  a la pregunta de opinión que se consideran como 'opinion positiva" con el objetivo de ordenarlas
      #'  de acuerdo a un gradiente de opinión
      #' @param grupo_negativo Vector tipo caracter. Indica los valores únicos de las variables asociadas
      #'  a la pregunta de opinión que se consideran como 'opinion negativa" con el objetivo de ordenarlas
      #'  de acuerdo a un gradiente de opinión
      #' @param orden_resp Vector tipo caracter que contiene los valores posibles de las respuestas
      #'  de preguntas asociadas a la opinión.
      #' @param colores_opinion Vector tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena'cada proporción de opinión.
      #' @param color_nsnc Valor tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena' la proporción asociada a la respuesta 'No sabe o no contesta".
      #' @param color_burbuja Valor tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena' el gráfico relacionado a los resultados de la pregunta de conocimiento.
      #' @param size_caption_opinion Valor tipo entero. Determina el tamano del texto mostrado en el
      #'  [caption] de la visualizacion principal. Es el parámetro [size] de la función
      #'  [ggplot2::element_text()] aplicada al argumento [plot.caption] del [ggplot2::theme()].
      #' @param size_caption_nsnc Valor tipo entero. Determina el tamano del texto mostrado en el
      #'  [caption] de la visualizacion relacionada a la respuesta 'No sabe o no contesta". Es el
      #'  parámetro [size] de la función [ggplot2::element_text()] aplicada al argumento
      #'  [plot.caption] del [ggplot2::theme()].
      #' @param caption_burbuja Valor tipo entero. Determina el tamano del texto mostrado en el
      #'  [caption] de la visualizacion relacionada al conocimiento de la persona. Es el
      #'  parámetro [size] de la función [ggplot2::element_text()] aplicada al argumento
      #'  [plot.caption] del [ggplot2::theme()].
      #' @param size_text_cat Valor tipo entero. Determina el tamano del texto mostrado en las
      #'  etiquetas relacionadas a la opinión de personajes. Es el parámetro [size] de la función
      #'  [ggplot2::element_text()] aplicada al argumento [axis.text.y] del [ggplot2::theme()].
      #' @param size_pct Valor tipo entero. Determina el tamano del texto mostrado dentro de las
      #'  barras que representan las proporciones de opinion. Es el parámetro [size] de la función
      #'  [ggfittext::geom_fit_text()].
      #' @param burbuja `LOGICAL`. Determina si mostrar o no el gráfico relacionado a los resultados
      #'  de la pregunta de conocimiento.
      #' @param salto Valor tipo entero. Reduce o aumenta la longitud de las categorías en el eje x.
      #' @param salto_respuestas Valor tipo entero. Reduce o aumenta la longitud de las categorías
      #'  posibles de respuestas de las preguntas de opinion.
      #' @param mostrar_nsnc `LOGICAL`. Determina si mostrar o no los resultados de los valores
      #'  "No sabe o no contesta".
      #' @param orden_cat Vector ordenado tipo caracter. Son los sufijos que las variables asociadas
      #'  a las preguntas y ordena los resultados de acuerdo a dicho vector.
      candidatoOpinion = function(patron_inicial,
                                  aspectos,
                                  ns_nc = "Ns/Nc",
                                  regular = "Regular",
                                  llave_burbuja = NA,
                                  filtro_burbuja = "respuesta == 'Sí'",
                                  size_burbuja = 8,
                                  grupo_positivo,
                                  grupo_negativo,
                                  orden_resp,
                                  colores_opinion = c("red", "yellow", "green"),
                                  color_nsnc = "gray70",
                                  color_burbuja = "blue",
                                  caption_opinion = "",
                                  caption_nsnc = "Ns/Nc",
                                  caption_burbuja = "Conocimiento",
                                  size_text_legend = 12,
                                  size_caption_opinion = 12,
                                  size_caption_nsnc = 14,
                                  size_caption_burbuja = 14,
                                  size_text_cat = 16,
                                  size_pct = 12,
                                  burbuja = T,
                                  salto = 20,
                                  salto_respuestas = 100,
                                  mostrar_nsnc = T,
                                  orden_cat = NULL){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        if(!is.na(llave_burbuja)){

          bd_burbuja <- analizar_frecuencias_aspectos(diseno = diseno,
                                                      diccionario = self$diccionario,
                                                      patron_pregunta = llave_burbuja,
                                                      aspectos = aspectos) %>%
            filter(eval(rlang::parse_expr(filtro_burbuja))) %>%
            left_join(self$diccionario %>% select(aspecto = llaves, tema))
        } else {
          bd_burbuja <- NA
        }

        analizar_frecuencias_aspectos(diseno = diseno,
                                      diccionario = self$diccionario,
                                      patron_pregunta = patron_inicial,
                                      aspectos = aspectos) |>
          left_join(self$diccionario %>%
                      select(aspecto = llaves, tema)) %>%
          graficar_candidato_opinion(ns_nc = ns_nc,
                                     regular = regular,
                                     grupo_positivo= grupo_positivo,
                                     grupo_negativo = grupo_negativo,
                                     caption_opinion = caption_opinion,
                                     caption_nsnc = caption_nsnc,
                                     caption_burbuja = caption_burbuja,
                                     size_text_legend = size_text_legend,
                                     size_caption_opinion = size_caption_opinion,
                                     size_caption_nsnc = size_caption_nsnc,
                                     size_caption_burbuja = size_caption_burbuja,
                                     size_text_cat = size_text_cat,
                                     size_pct = size_pct,
                                     orden_resp = orden_resp,
                                     colores = colores_opinion,
                                     color_nsnc = color_nsnc,
                                     burbuja = bd_burbuja,
                                     color_burbuja = color_burbuja,
                                     size_burbuja = size_burbuja,
                                     salto = salto,
                                     tema = self$tema,
                                     mostrar_nsnc = mostrar_nsnc,
                                     salto_respuestas = salto_respuestas,
                                     orden_cat = orden_cat,
                                     patron_inicial = patron_inicial)

      },
      #' @description Visualiza los resultados de preguntas con mismas posibles respuestas y aplicadas
      #'  sobre diferentes tópicos. Usualmente este tipo de preguntas son asociadas a la opinión y/o
      #'  el conocimiento de personajes. El conjunto de preguntas están identificas, por ejemplo, como
      #'  conocimiento_persona1, conocimiento_persona2, etc y opinion_persona1, opinion_persona2, etc.
      #'  Por lo general, las variables de opinion destán condicionadas por las de conocimiento. Esto
      #'  está considerado en el procesamiento de los resultados. La vsualizacion es en forma de
      #'  tabla usando la paquetería [flextable].
      #' @param patron_opinion Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables que representan la opinion. En el ejemplo este parámetro seía
      #'  "opinion".
      #' @param aspectos Vector tipo caracter que identifica a las diferentes variables a procesar. De
      #'  acuerdo al método, el ejemplo sería c("persona1", "persona2").
      #' @param ns_nc Valor tipo caracter. Es el valor asociado a la respuesta  'No sabe o no contesta'
      #'  en la pregunta de opinión. Ya que este valor no aporta información, se separa de los resultados.
      #' @param regular Valor tipo caracter. Es el valor asociado a la respuesta  'Regular' que
      #'  representa una opinión neutra.
      #' @param patron_conocimiento Valor tipo caracter. Es el patron inicial de caracteres que comparten
      #'  los nombres de las variables que representan el conocimiento En el ejemplo este parámetro seía
      #'  "opinion".
      #' @param filtro_conocimiento Valor tipo caracter. Operación de identidad que se aplica como filtro
      #'  entre los resultados de las preguntas asociadas al conocimiento. Por lo general el filtro
      #'  tiene la forma filtro = "respuesta == 'Sí'". Este parámetro no afecta al cálculo de las
      #'  estimaciones de las variables asociadas a la opinion, unicamente a las asociadas al conocimiento.
      #' @param orden_opinion Vector tipo caracter que contiene los valores posibles de las respuestas
      #'  de preguntas asociadas a la opinión. Las columnas de la tabla se orden de acuerdo a este vector
      #' @param etiquetas Dupla ordenada tipo caracter. Nombres de las variables para mostrar en el
      #'  resultado final.
      #' @param colores_opinion Vector tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena'cada proporción de opinión. Tantos colores como opiniones posibles.
      #' @param colores_candidato Vector que identifica los colores asociados a los valores únicos de la
      #'  variable relacionada al candidato.
      #' @param color_principal Valor tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena'la parte principal de la tabla
      #' @param color_conocimiento Valor tipo caracter. Color en formato `HEX`. Es el color con el que
      #'  se 'rellena'la columna de la tabla relacionada con los resultados de conocimiento.
      #' @param size_text_header Valor entero. Define el tamano del texto del encabezado
      #' @param size_text_body Valor entero. Define el tamano del texto del cuerpo de la tabla
      #' @param salto Valor entero. Define la longitud del texto de las filas
      #' @param salto_respuestas Valor entero. Define la longitu del texto de los encabezadps.
      candidatoOpinion2 = function(patron_opinion,
                                   aspectos,
                                   ns_nc = "Ns/Nc",
                                   regular = "Regular",
                                   patron_conocimiento,
                                   filtro_conocimiento = "respuesta == 'Sí'",
                                   orden_opinion,
                                   etiquetas = c("Candidato", "Opinión"),
                                   colores_opinion = c("red", "yellow", "green"),
                                   colores_candidato = colores_candidato,
                                   color_principal = "#BF498A",
                                   color_conocimiento = "blue",
                                   size_text_header = 18,
                                   size_text_body = 14,
                                   salto = 20,
                                   salto_respuestas = 100){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        calcular_tabla_candidatoOpinion(diseno = diseno,
                                        diccionario = self$diccionario,
                                        patron_opinion = patron_opinion,
                                        patron_conocimiento = patron_conocimiento,
                                        aspectos = aspectos,
                                        filtro_conocimiento = filtro_conocimiento,
                                        orden_opinion = orden_opinion,
                                        ns_nc = ns_nc,
                                        salto_respuestas = salto_respuestas) %>%
          formatear_tabla_candidatoOpinion(orden_opinion = orden_opinion,
                                           etiquetas = etiquetas,
                                           colores_opinion = colores_opinion,
                                           color_principal = color_principal,
                                           colores_candidato = colores_candidato,
                                           size_text_header = size_text_header,
                                           size_text_body = size_text_body,
                                           salto = salto,
                                           color_conocimiento = color_conocimiento)

      },
      candidatoPartido = function(llave_partido,
                                  llave_conocimiento,
                                  respuesta_conoce,
                                  candidatos,
                                  corte_otro,
                                  cliente,
                                  colores_candidatos,
                                  colores_partido,
                                  corte_vis = 0.0,
                                  size_text = 6,
                                  size_text_conocimiento = 6){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        analizar_candidatoPartido(diseno = diseno,
                                  diccionario = self$diccionario,
                                  llave_partido = llave_partido,
                                  llave_conocimiento = llave_conocimiento,
                                  respuesta_conoce = respuesta_conoce,
                                  candidatos = candidatos,
                                  corte_otro = corte_otro) %>%
          purrr::map(~.x %>%
                       left_join(self$diccionario %>% select(aspecto = llaves, tema))
          ) %>%
          graficar_candidatoPartido(cliente = cliente,
                                    tipo_conoce = "intervalos",
                                    colores_candidato = colores_candidatos,
                                    colores_partido = colores_partido,
                                    solo_respondidos = T,
                                    tema = self$tema,
                                    corte_vis = corte_vis,
                                    size_text = size_text,
                                    size_text_conocimiento=size_text_conocimiento)
      },
      candidatoSaldo = function(llave_opinion,
                                candidatos,
                                positivos,
                                negativos,
                                regular = "Regular",
                                ns_nc = "Ns/Nc",
                                color_positivo = "green",
                                color_negativo = "red",
                                orden_cat = NULL,
                                salto_cat = 25,
                                caption_opinion,
                                size_text_cat = 14,
                                size_text_legend = 10,
                                size_pct = 12,
                                size_caption_opinion = 10){

        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }

        bd_saldo <- analizar_saldoOpinion(diseno = diseno,
                                          diccionario = self$diccionario,
                                          llave_opinion = llave_opinion,
                                          candidatos = candidatos,
                                          grupo_positivo = positivos,
                                          grupo_negativo = negativos)

        bd_saldo_aux <-
          analizar_frecuencias_aspectos(diseno = diseno,
                                        diccionario = self$diccionario,
                                        patron_pregunta = llave_opinion,
                                        aspectos = candidatos) |>
          left_join(self$diccionario %>%
                      select(aspecto = llaves, tema), by = "aspecto")

        if(!is.null(ns_nc)){
          bd <- bd_saldo_aux %>% group_by(tema) %>% complete(respuesta = ns_nc, fill = list(media = 0)) %>% ungroup
        }

        grupo_negativo = negativos
        grupo_positivo = positivos

        ejemplo <- bd %>%
          filter(respuesta != regular) |>
          mutate(respuesta = case_when(respuesta %in% positivos ~ "Positiva",
                                       respuesta %in% negativos ~ "Negativa",
                                       .default = respuesta)) |>
          group_by(tema, respuesta) |>
          summarise(media = sum(media, na.rm = TRUE),
                    .groups = "drop") |>
          mutate(media = dplyr::if_else(condition = respuesta == "Negativa",
                                        true = -media,
                                        false = media)) |>
          mutate(etiqueta = scales::percent(abs(media), 1),
                 media = if_else(respuesta %in% grupo_negativo, -1*media, media),
                 media = if_else(respuesta == regular, media/2, media)) %>%
          group_by(tema) %>%
          mutate(saldo = sum(as.numeric(!(respuesta %in% c(regular, ns_nc)))*media))

        orden <- ejemplo %>% arrange(saldo) %>% pull(tema) %>% unique %>% na.omit

        if(!is.null(orden_cat)) {

          orden <- aux %>%
            ungroup() |>
            mutate(aspecto = gsub(pattern = paste0(patron_inicial, "_"),
                                  replacement = "",
                                  x = aspecto)) |>
            distinct(aspecto, tema) |>
            mutate(aspecto = factor(aspecto, levels = orden_cat, ordered = TRUE)) |>
            arrange(desc(aspecto)) |>
            pull() |>
            as.factor()

        }

        ejemplo %>%
          {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
          mutate(respuesta = factor(respuesta, levels = c("Negativa", "Positiva"))) |>
          graficar_barras_saldo(orden = orden,
                                grupo_positivo = "Positiva",
                                grupo_negativo = "Negativa",
                                Regular = NA_character_,
                                colores = c("Positiva" = color_positivo,
                                            "Negativa" = color_negativo),
                                salto_respuestas = 50,
                                salto_tema = salto_cat,
                                caption_opinion = caption_opinion,
                                size_text_cat = size_text_cat,
                                size_pct = size_pct,
                                size_caption_opinion = size_caption_opinion,
                                size_text_legend = size_text_legend)

      },
      metodo_morena = function(personajes, atributos){
        if(is.null(self$diseno)) {

          diseno <- self$encuesta$muestra$diseno

        } else {

          diseno <- self$diseno

        }
        analizar_morena(diseno = diseno, diccionario = self$diccionario, personajes = personajes, atributos = atributos) %>%
          graficar_morena(personajes = personajes, atributos = atributos)
      }
    ))

#'Esta es la clase de Regiones
#'@export
#'
Regiones <- R6::R6Class(classname = "Regiones",
                        public = list(
                          encuesta = NULL,
                          diseno = NULL,
                          diccionario = NULL,
                          tema = NULL,
                          shp_regiones = NULL,
                          initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema){
                            self$encuesta <- encuesta
                            self$diseno <- diseno
                            self$diccionario <- diccionario
                            self$tema <- tema
                            self$crear_shp_regiones()
                          },
                          mapa_ganador = function(variable,region = 'region', lugar = 1, na_rm = T){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            self$shp_regiones|>
                              left_join(
                                analizar_cruce(diseno = diseno,
                                               variable_principal = region,
                                               variable_secundaria = variable,
                                               vartype = 'cv',na_rm = na_rm)|>
                                  group_by(region)|>
                                  filter(dense_rank(-coef) == lugar)|>
                                  select(- c('_cv','pres')),
                                by = 'region')|>
                              filter(!is.na(region))%>%
                              graficar_mapaRegiones(variable = {{variable}})
                          },
                          mapa_degradadoNumerico = function(variable){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            analizar_frecuenciasRegion(regiones = self$shp,
                                                       variable = {{variable}},
                                                       diseno = diseno) %>%
                              graficar_mapaRegiones(variable = {{variable}}, categorica = F)
                          },
                          heatmap_conocimiento = function(patron_llaveConocimiento, candidatos, respuesta, ordenRegiones = NULL, salto_labelRegiones = 5){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            bd_analizar_conocimientoRegion <- analizar_conocimientoRegion(patron_llaveConocimiento = patron_llaveConocimiento,
                                                                                          aspectos_llaveConocimiento = candidatos,
                                                                                          filtro_respuestaConocimiento = respuesta,
                                                                                          diseno = diseno,
                                                                                          diccionario = self$diccionario)
                            graficar_conocimientoRegion(bd = bd_analizar_conocimientoRegion, ordenRegiones = ordenRegiones, salto_labelRegiones = salto_labelRegiones)
                          },
                          heatmap_saldoOpinion = function(patron_llaveOpinion, candidatos, ns_nc, cat_negativo, cat_regular, cat_positivo,
                                                          ordenRegiones = NULL, salto_labelRegiones = 5){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            analizar_saldoRegion(patron_llaveOpinion = patron_llaveOpinion, aspectos_llaveOpinion = candidatos, ns_nc = ns_nc, cat_negativo = cat_negativo, cat_regular = cat_regular, cat_positivo = cat_positivo, diseno = diseno, diccionario = self$diccionario) %>%
                              graficar_saldoRegion(ordenRegiones = ordenRegiones, salto_labelRegiones = salto_labelRegiones)

                          },
                          mapa_resaltarRegion = function(region, color){
                            self$shp_regiones %>%
                              mutate(color = dplyr::if_else(condition = region %in% !!region,
                                                            true = color,
                                                            false = "gray70")) %>%
                              ggplot(aes(fill = color), color = "black") +
                              geom_sf() +
                              scale_fill_identity() +
                              theme_void() +
                              labs(tit = region)
                          },
                          crear_shp_regiones = function(){
                            sf_use_s2(T)
                            self$shp_regiones <-
                              self$encuesta$shp_completo$shp$SECCION %>%
                              # Corrige geometrias validas en el plano pero invalidas para s2
                              #  (vertices duplicados) que rompen la disolucion por region
                              sf::st_make_valid() %>%
                              left_join(self$encuesta$muestra$muestra$poblacion$marco_muestral %>%
                                          distinct(SECCION, region), by = "SECCION") %>%
                              group_by(region) %>%
                              summarise(n()) %>%
                              sf::st_buffer(dist = 0)
                            sf_use_s2(F)
                          },
                          resaltar_region = function(color){
                            if(is.null(self$encuesta$muestra$region)){
                              stop("Correr clase$muestra$diseno_region para indicar la region seleccionada")
                            }
                            self$encuesta$Resultados$Regiones$shp_regiones |>
                              mutate(color = if_else(region %in% self$encuesta$muestra$region, color, "gray70")) %>%
                              ggplot(aes(fill = color)) +
                              geom_sf(color = "black") +
                              scale_fill_identity() +
                              theme_void() +
                              labs(tit = self$encuesta$muestra$region)
                          }
                        ))

#'Esta es la clase de Tendencias
#'@export
#'
Tendencias <- R6::R6Class(classname = "Tendencias",
                          public = list(
                            encuesta = NULL,
                            diseno = NULL,
                            bd_resultados = NULL,
                            initialize = function(encuesta = NULL, diseno = NULL){
                              self$encuesta <- encuesta

                              self$diseno <-
                                {
                                  if(!is.null(encuesta)) {
                                    self$encuesta$muestra$diseno
                                  } else {
                                    diseno
                                  }
                                }

                              self$bd_resultados <- self$diseno$variables |>
                                mutate(peso = weights(self$diseno))
                            },
                            intencion_voto = function(variable, valores_interes, colores, sin_peso = T, linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-
                                calcular_mediaMovil(bd_resultados = self$bd_resultados,
                                                    variable = variable,
                                                    valores_interes = valores_interes,
                                                    sin_peso = sin_peso) |>
                                rename(pct = !!rlang::sym(paste0("movil_", variable)))
                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = !!rlang::sym(variable))) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Intención de voto", color = "") +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                {if(!sin_peso & linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                       color = "red", linetype = "dashed", size = 1)}+
                                scale_color_manual(values = colores) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted"),
                                      legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            conocimiento = function(variables, colores, sin_peso = T, valores_interes = "Sí", linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil<- variables|>
                                purrr::map(~{
                                  var_aux <- .x
                                  calcular_mediaMovil(bd_resultados = self$bd_resultados,
                                                      variable = .x,
                                                      valores_interes = valores_interes,
                                                      sin_peso = sin_peso)|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_gen') )
                                })|>
                                reduce(left_join, by = 'hora' )|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_gen.*',variables[1],')')))|>
                                select(-matches('pos_gen_.*_'))|>
                                tidyr::pivot_longer(cols = contains('movil_'),
                                                    names_to = 'variable',
                                                    values_to = 'pct')

                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = variable)) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Conocimiento", color = "") +
                                scale_color_manual(values = purrr::set_names(colores, paste0("movil_", variables)),
                                                   labels = purrr::set_names(variables, paste0("movil_", variables))) +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                {if(!sin_peso & linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                       color = "red", linetype = "dashed", size = 1)}+
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted"),
                                      legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            intencion_voto_region = function(variable, valores_interes, colores, sin_peso = T, variable_region = "region",  linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-
                                calcular_mediaMovil_region(bd_resultados = self$bd_resultados,
                                                           variable = variable,
                                                           valores_interes = valores_interes,
                                                           variable_region = variable_region,
                                                           sin_peso = sin_peso) |>
                                rename(pct = !!rlang::sym(paste0("movil_", variable)))
                              g <-
                                bd_mediaMovil|>
                                ggplot(aes(x = hora, y = pct, color = !!rlang::sym(variable))) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Intención de voto", color = "") +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                scale_color_manual(values = colores) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted")) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                # {if(sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                          color = "blue", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                # {if(!sin_peso)
                                # geom_vline(aes(xintercept =  pos_reg_gen_snp),
                                #            color = "blue", size = 1)}+
                                {if(!sin_peso &  linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                        color = "red", linetype = "dashed", size = 1)}+
                                # {if(!sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                           color = "blue", linetype = "dashed", size = 1)}+
                                facet_wrap(as.formula(paste0("~", variable_region)))+
                                theme(legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            conocimiento_region = function(variables, colores, sin_peso = T, valores_interes = "Sí", variable_region = "region",  linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-variables|>
                                purrr::map(~{
                                  var_aux <- .x
                                  calcular_mediaMovil_region(bd_resultados = self$bd_resultados,
                                                             variable = .x,
                                                             valores_interes = valores_interes,
                                                             variable_region = variable_region,
                                                             sin_peso = sin_peso)|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_gen') )|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_reg_gen') )

                                })|>
                                reduce(left_join, by = c(variable_region,'hora') )|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_gen.*',variables[1],')')))|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_reg_gen.*',variables[1],')')))|>
                                select(-matches('pos_gen_.*_'),-matches('pos_reg_gen_.*_') )|>
                                tidyr::pivot_longer(cols = contains('movil_'),
                                                    names_to = 'variable',
                                                    values_to = 'pct')

                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = variable)) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Conocimiento", color = "") +
                                scale_color_manual(values = purrr::set_names(colores, paste0("movil_", variables)),
                                                   labels = purrr::set_names(variables, paste0("movil_", variables))) +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted")) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                # {if(sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                          color = "blue", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                # {if(!sin_peso)
                                #   geom_vline(aes(xintercept =  pos_reg_gen_snp),
                                #              color = "blue", size = 1)}+
                                {if(!sin_peso &  linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                        color = "red", linetype = "dashed", size = 1)}+
                                # {if(!sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                           color = "blue", linetype = "dashed", size = 1)}+
                                facet_wrap(as.formula(paste0("~", variable_region)))+
                                theme(legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            }
                          ))

#'Esta es la clase de Modelo
#'@export
#'
Modelo <- R6::R6Class(classname = "Modelo",
                      public = list(
                        encuesta = NULL,
                        diseno = NULL,
                        diccionario = NULL,
                        tema = NULL,
                        graficadas = NULL,
                        initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
                          self$encuesta <- encuesta
                          self$diseno <- diseno
                          self$diccionario <- diccionario
                          self$tema <- tema
                          self$graficadas <- graficadas
                        },
                        correspondencia = function(var1, var2, legenda1 = NULL, legenda2 = NULL, colores = NULL){


                          analisis_correspondencia(var1 = var1, var2 = var2, legenda1 = legenda1, legenda2 = legenda2, diseno = self$diseno, colores = colores)
                        },
                        componentesPrincipales = function(variables){


                          pc <- survey::svyprcomp(survey::make.formula(variables),
                                                  design = self$diseno,
                                                  scale=TRUE, scores=TRUE)
                          factoextra::fviz_pca_biplot(pc, geom.ind = "point", labelsize = 2, repel = T)}
                        #   ,
                        #   blackBox = function(vars, stimuli){
                        #
                        #     if(is.null(self$diseno)) {
                        #
                        #       diseno <- self$encuesta$muestra$diseno
                        #
                        #     } else {
                        #
                        #       diseno <- self$diseno
                        #
                        #     }
                        #
                        #     diseno$variables %>%
                        #       as_tibble() |>
                        #       analizar_blackbox_1d(vars, stimuli) %>%
                        #       graficar_blackbox_1d()
                        #
                        #   }
                        # )
                      ))
