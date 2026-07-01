#' Clase Encuesta
#'
#' @description La clase encuesta es la interfaz de usuario de la paquetería encuestar.
#'  A través de la clase encuesta se producen los resultados y se puede consultar información
#'  relacionada al levantamiento, respuestas y equipo que trabaja en campo. La clase Encuesta tiene
#'  cinco clases subordinadas: `Cuestionario`, `Respuestas`, `Muestra`, `Resultados` y `Auditoria`.
#'
#' @field muestra El campo `muestra` contiene los resultados de ejecutar la clase `Muestra` de
#'  jerarquía menor. Dicha clase contiene el diseño muestral calculado a partir de la edad, sexo y
#'  geolocalización de los individuos entrevistados. Además del diseño muestral contiene métodos para
#'  modificar dicho diseño a partir de postestratificaciones o filtrar a través de subconjuntos de
#'  la muestra.
#' @field shp El campo `shp` contiene la cartografía de los clusters que hayan sido seleccionados en
#'  el proceso de muestreo.
#' @field cuestionario El campo `cuestionario` contiene los resultados de ejecutar la clase
#'  `Cuestionario`. Dicha clase originalmente recibía el texto y las clases asociadas al cuestinoario
#'  en formato .docx y generaba el diccionario. Actualmente la clase recibe y asigna el [tibble()]
#'  que contiene el diccionario y realiza unas cuantas verificaciones.
#' @field respuestas El campo `respuestas` contiene los resultados de ejecutar la clase `Respuestas`
#'  de jerarquía menor. Dicha clase tiene como objetivo verificar, limpiar y estandarizar las
#'  respuestas recibidas de campo.
#' @field n_simulaciones Valor entero usado para simular un [tibble()] necesario para generar
#'  la clase `Respuestas`.
#' @field opinometro_id Valor entero usado para generar el [tibble()] de respuestas a través
#'  de la plataforma Opinómetro y que es necesario para generar la clase `Respuestas`.
#' @field pool El campo `pool` se hereda para ser usado por la clase `Opinómetro`.
#' @field bd_categorias El [tibble()] que contiene las categoriías generadas por IA se une al
#'  [tibble()] `respuestas` independientemente si el segundo es del campo `respuestas` o
#'  generado por opinómetro.
#' @field bd_correcciones El campo se hereda a la clase `Respuestas`.
#' @field patron El campo se hereda a la clase `Respuestas`.
#' @field auditoria_telefonica El campo se hereda a la clase `Respuestas`.
#' @field quitar_vars `DEPRECATED` en futuro desuso.
#' @field mantener El campo se hereda a la clase `Respuestas`.
#' @field auditar El campo se queda guardado en la clase `Encuesta` para su uso en la aplicación
#'  de monitoreo del levantamiento.
#' @field vars_tendencias El campo se queda guardado en la clase `Encuesta` para su uso en la
#'  aplicación de monitoreo del levantamiento.
#' @field sin_peso El campo se hereda a la clase `Respuestas`.
#' @field rake El campo se hereda a la clase `Respuestas`.
#' @field mantener_falta_coordenadas El campo se hereda a la clase `Respuestas`.
#' @field tipo_encuesta Por defecto se imputa que el tipo de encuesta sea `ine`.
#' @field shp_completo Extensión del campo `shp`. Una vez calculadas las entrevistas efectivas y
#'  asignadas las ponderaciones. Se determinan las ubicaciones puntuales donde se levantaron las
#'  entrevistas y se agrega al objeto `shp`.
#' @field Resultados Clase subordinada. La clase `Resultados` contiene todos los método utilizados
#'  para generar el entregable final.
#' @field Auditoria Clase subordinada. La clase `Auditoria` en su mayor parte escribe el script de
#'  la aplicación de monitorio de en un `folder` llamado `auditoria` en el `working directory`.
#'
#' @export
#' @import dplyr ggplot2 tidyr sf purrr stringr
Encuesta <-
  R6::R6Class(
    classname = "Encuesta",
    public = list(
      muestra = NULL,
      shp = NULL,
      cuestionario = NULL,
      respuestas = NULL,
      n_simulaciones = NULL,
      opinometro_id = NULL,
      pool = NULL,
      bd_categorias = NULL,
      bd_correcciones = NULL,
      patron = NA,
      auditoria_telefonica = NULL,
      quitar_vars = NULL,
      mantener = NULL,
      auditar = NULL,
      vars_tendencias = NULL,
      sin_peso = NULL,
      rake = NULL,
      mantener_falta_coordenadas = NULL,
      tipo_encuesta = NULL,
      shp_completo = NULL,
      Opinometro = NULL,
      Resultados = NULL,
      Auditoria = NULL,
      #' @description Se reciben los insumos de respuestas, auditoria y otros parámetros asociados
      #'  al levantamiento de la encuesta para construir el diseño muestral y las clases posteriores
      #'  para generar resultados.
      #' @param muestra Objeto tipo `.rda` generado por la paquetería `muestrear`. Es el diseño muestral
      #'  de la encuesta. El dobjeto `muestra` es de formato personalizado por lo que no es posible
      #'  (o práctico) sustituirlo.
      #' @param shp Objeto tipo `.rda` generado por la paquetería `muestrear`. Contiene toda la
      #'  cartografía utilizada para el equipo de campo y que a su vez es utilizada por la aplicación
      #'  de monitoreo del levantamiento. El objeto `shp` es de formato personalizado por lo que no
      #'  es posible (o práctico) sustituirlo.
      #' @param cuestionario [tibble()] que es el diccionario del cuestionario que se aplica a los
      #'  entrevistados. El parámetro `cuestionario`, después de una actualización, actúa como el
      #'  diccionario (o codebbok).
      #' @param respuestas [tibble()] de respuestas obtenidas por las personas entrevistadas.
      #' @param n_simulaciones Valor entero. Con el objetivo de publicar la aplicación de monitoreio
      #'  antes de inciar el levantamiento es posible simular una base de respuestas.
      #'  `n_simulaciones` es el total de respuestas simuladas, es decir, es el [nrow()] que simula
      #'  al campo `respuestas`. El valor recomendado es del 10% del total de entrevistas objetivo
      #'  de la encuesta. Es mutuamente excluyente con el campo `respuestas`.
      #' @param opinometro_id Valor entero. Identificador del cuestionario aplicado en campo generado
      #'  en la plataforma `Opinómetro`. Es necesario consultar a la persona que construyó el
      #'  cuestionario para conocer el `opinometro_id` asociado. Es mutuamente excluyente con el
      #'  campo `respuestas`.
      #' @param pool Objeto tipo [pool] generado al conectarse a la base de datos que almacena las
      #'  respuestas. Se recomienda utilizar la función [dbPool()] de la paquetería [pool] para esto.
      #' @param bd_categorias [tibble()] que contiene los resultados generados por IA a partir de las
      #'  preguntas abiertas.
      #' @param bd_correcciones [tibble()] que contiene las correcciones de los registros de las encuestas
      #'  efectivas.
      #' @param patron Valor tipo caracter que indica qué cadenas de texto quitar de las posibles opciones
      #'  de respuesta a las preguntas para no presentarlas en los resultados.
      #' @param auditoria_telefonica [tibble()] que contiene las entrevistas que, por auditoría telefónica,
      #'  han sido eliminadas de los registros.
      #' @param quitar_vars `DEPRECATED` Vector tipo caracter que contiene las variables que se desean
      #'  omitir del procesamiento.
      #' @param mantener Vector tipo caracter que indica los clusters a los cuales habrá que forzar las
      #'  entrevistas que se hayan levantado cerca de los mismos.
      #' @param auditar Vector tipo caracter que contiene las varaibles que se mostraán en la aplicación
      #'  de monitoreo de levantamiento en la pestala `Resultados`.
      #' @param vars_tendencias Vector tipo caracter que contiene las variables que se mostrarán en la
      #'  aplicación de monitoreo de levantamiento en la sección `Tendencias`.
      #' @param sin_peso `LOGICAL` Determina si esl diseno muestral construido será ponderado o no.
      #' @param rake `LOGICAL` Determina si el diseno muestral será postestratificado por edad y sexo.
      #' @param mantener_falta_coordenadas `LOGICAL`. Determina si se descartan o no las entrevistas sin
      #'  geolocalización válida o nula.
      #' @param tipo_encuesta `DEPRECATED`. La paquetería cuenta con un modo de cálculo de diseño muestral
      #'  análogo al `INEGI`. Sin embargo, este modo ha entrado en desuso y en un futuro desaparecerá.
      initialize = function(muestra = NA,
                            shp = NA,
                            cuestionario = NA,
                            respuestas = NA,
                            n_simulaciones = NA,
                            opinometro_id = NA,
                            pool = NA,
                            bd_categorias = NULL,
                            bd_correcciones = NULL,
                            patron = NA,
                            auditoria_telefonica = tibble(SbjNum = c(000),
                                                          razon = c("A")),
                            quitar_vars = c(),
                            mantener = "",
                            auditar = c(""),
                            vars_tendencias = NA,
                            sin_peso = FALSE,
                            rake = TRUE,
                            mantener_falta_coordenadas = FALSE,
                            tipo_encuesta = "ine") {
        sf_use_s2(F)
        tipo_encuesta <- match.arg(tipo_encuesta, c("inegi","ine"))
        self$sin_peso <- sin_peso
        self$quitar_vars <- quitar_vars
        self$rake <- rake
        self$tipo_encuesta <- tipo_encuesta
        self$patron <- patron
        self$auditar <- auditar
        self$vars_tendencias <- vars_tendencias
        self$mantener_falta_coordenadas <- mantener_falta_coordenadas
        self$n_simulaciones <- if("logical" %in% class(respuestas)) n_simulaciones else 0
        self$opinometro_id <- opinometro_id
        self$pool <- pool
        self$bd_categorias <- bd_categorias


        # Valorar si no es mejor un active binding
        un <- muestra$niveles %>% filter(nivel == muestra$ultimo_nivel)
        nivel <- un %>% unite(nivel, tipo, nivel) %>% pull(nivel)
        var_n <- un %>% pull(variable)

        # Se comprueba el metodo de obtencion de datos
        if(("data.frame" %in% class(respuestas)) & !is.na(self$opinometro_id)   ){
          print("No se puede cargar la base del Opinometro y una base externa, verifique el método de obtención de bases")
          stop()
        }

        # Valorar active binding
        self$cuestionario <- Cuestionario$new(documento = cuestionario, patron)
        # Valorar active binding
        self$auditoria_telefonica <- auditoria_telefonica %>% distinct(SbjNum, .keep_all = T)

        # Base de respuestas incorrectas y su correccion
        self$bd_correcciones <- bd_correcciones

        self$shp_completo <- shp

        self$shp <-
          shp$shp %>%
          purrr::pluck(var_n) %>%
          inner_join(muestra$muestra %>%
                       purrr::pluck(var_n) %>%
                       unnest(data) %>%
                       distinct(!!rlang::sym(var_n) := !!rlang::sym(var_n),
                                !!rlang::sym(nivel)))
        self$mantener <- mantener

        catalogo_variables <-
          catalogo_variables |>
          bind_rows(self$cuestionario$diccionario |>
                      select(variable = llaves) |>
                      mutate(plataforma = "cuestionario",
                             primer_nivel = "cuestionario",
                             segundo_nivel = dplyr::if_else(condition = variable %in% c("cluster",
                                                                                        "edad",
                                                                                        "sexo"),
                                                            true = "sistema",
                                                            false = "cuestionario")))


        if("data.frame" %in% class(respuestas)) {

          intentos_efectivos <-
            respuestas |>
            select(SbjNum, num_range("INT", 1:20)) |>
            mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
            tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
            filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
            mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
            select(SbjNum, intento_efectivo)

          geolocalizacion_efectiva <-
            purrr::pmap_df(.l = list(ids = intentos_efectivos %>% pull(SbjNum),
                                     intento_efectivo = intentos_efectivos %>% pull(intento_efectivo)),
                           .f = ~ obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = respuestas,
                                                                       id = ..1,
                                                                       intento_efectivo = ..2))

          respuestas <-
            respuestas |>
            left_join(geolocalizacion_efectiva, by = "SbjNum") |>
            mutate(Latitude = GPS_INT_LA,
                   Longitude = GPS_INT_LO)
            # transmute(across(c(all_of(catalogo_variables |>
            #                           filter(plataforma %in% c("surveytogo", "encuestar", "cuestionario"),
            #                                  segundo_nivel %in% c("sistema", "cuestionario")) |>
            #                           pull(variable)), "edad", "sexo")
            #                  )
            #           )

        }

        if(!is.na(opinometro_id)) {

          opinometro <- Opinometro$new(id_cuestionarioOpinometro = self$opinometro_id,
                                       pool = self$pool,
                                       diccionario = self$cuestionario$diccionario)
          opinometro$definir_contenido()
          opinometro$construir_respuestas()

          respuestas <- opinometro$bd_respuestas_cuestionario

        #  self$Opinometro <- opinometro


        }

        if(!is.null(bd_categorias)) {

          respuestas <-
            respuestas |>
            left_join(bd_categorias,
                      by = "SbjNum")

        }

        if(!("data.frame" %in% class(respuestas)) & !is.null(n_simulaciones)){
          respuestas <- self$simular_surveytogo(cuestionario = self$cuestionario,
                                                n = self$n_simulaciones,
                                                diseño = muestra,
                                                shp = shp)
        }

        self$respuestas <- Respuestas$new(base = respuestas %>% mutate(cluster_0 = SbjNum),
                                          encuesta = self,
                                          catalogo = catalogo_variables,
                                          mantener_falta_coordenadas = self$mantener_falta_coordenadas,
                                          muestra_completa = muestra,
                                          patron = patron,
                                          nivel = nivel,
                                          var_n = var_n)

        # Muestra (recalcula fpc)
        self$muestra <- Muestra$new(muestra = muestra,
                                    respuestas = self$respuestas$base,
                                    nivel = nivel,
                                    var_n = var_n)

        # Informacion muestral
        self$respuestas$vars_diseno(muestra = self$muestra, var_n = var_n, tipo_encuesta = self$tipo_encuesta)
        # Diseno
        self$muestra$extraer_diseno(respuestas = self$respuestas$base,
                                    marco_muestral = self$muestra$muestra$poblacion$marco_muestral,
                                    tipo_encuesta = self$tipo_encuesta,
                                    sin_peso = self$sin_peso,
                                    rake = self$rake)

        print(glue::glue("La base de campo contiene ", as.character(nrow(respuestas)), " filas"))
        print(glue::glue("La base de eliiminadas contiene ", as.character(nrow(self$respuestas$eliminadas)), " filas"))
        if(!is.null(self$respuestas$no_efectivas)  ){ print(glue::glue("La base de no efectivas contiene ", as.character(nrow(self$respuestas$no_efectivas)), " filas"))}
        print(glue::glue("La base de entrevistas efectivas contiene ", as.character(nrow(self$muestra$diseno$variables)), " filas"))

        #Preguntas
        self$Resultados <- Resultados$new(encuesta = self, diseno = NULL, diccionario = NULL, tema = tema_morant())

        # Shiny app de auditoria
        self$Auditoria <- Auditoria$new(encuesta = self, tipo_encuesta = self$tipo_encuesta)
        file.copy(overwrite = FALSE,
                  from = system.file("constantes_y_funciones/constantes.R",
                                     package = "encuestar",
                                     mustWork = TRUE),
                  to = "R")
        file.copy(overwrite = FALSE,
                  from = system.file("constantes_y_funciones/funciones.R",
                                     package = "encuestar",
                                     mustWork = TRUE),
                  to = "R")
        source(file = paste0(getwd(), "/R/constantes.R"))
        beepr::beep()
        print(glue::glue("Las siguientes variables no son de sistema, plataforma o están en el diccionario"))
        print(match_dicc_base(self), n = Inf)
      },
      #' @description
      #' Simula una base de respuestas de campo a parti del diccionario delos insumos mínumos necesarios
      #'  y suficientes.
      #' @method simular_surveytogo Encuesta
      #' @param cuestionario Campo de la clase encuesta.
      #' @param n Valor entero. Define el [nrow()] del [tibble()] de respuestas simuladas.
      #' @param diseño Parámetro inicial de la clase `Encuesta`.
      #' @param shp Parámetro inicial de la clase `Encuesta`.
      simular_surveytogo = function(cuestionario, n, diseño, shp){
        #simular respuestas
        respuestas <- cuestionario$diccionario %>% mutate(n = n) %>%
          pmap_dfc(function(llaves, respuestas, tipo_pregunta, n,...){
            if(tipo_pregunta == "numericas") {
              aux_r <- respuestas[1] %>%
                str_split(pattern = "-") %>%
                pluck(1) %>%
                as.numeric()

              respuestas <- seq(aux_r[1], aux_r[2]) %>%
                as.character() %>%
                c(respuestas[2])
            }

            tibble(llaves = sample(respuestas,size = n, replace = T)) %>%
              set_names(llaves)
          })
        #ubicación aleatoria en muestra
        secc <- diseño$poblacion$marco_muestral %>%
          semi_join(diseño$muestra$MZA %>%
                      distinct(cluster_2),
                    by = "cluster_2") %>%
          distinct(cluster_2,SECCION)

        respuestas <- shp$shp$SECCION %>%
          semi_join(secc) %>%
          st_sample(size = n) %>%
          st_coordinates() %>%
          as_tibble %>%
          sf::st_as_sf(coords = c("X","Y"), crs = "+init=epsg:4326") %>%
          st_join(shp$shp$SECCION) %>%
          select(SECCION) %>%
          left_join(secc) %>%
          transmute(SECCION = as.character(cluster_2)) %>%
          bind_cols(st_coordinates(.)) %>%
          as_tibble() %>% select(-geometry) %>% rename(Longitude = X, Latitude = Y) %>% bind_cols(respuestas) %>%
          tibble::rownames_to_column(var = "SbjNum") %>% mutate(SbjNum = as.numeric(SbjNum))
        #simular sexo y edad para postestratificación
        respuestas <- respuestas %>%
          mutate(sexo = sample(c("Hombre", "Mujer"),
                               size = n, replace = T),
                 edad = sample(18:100,size = n, replace = T))
        #formato de base
        respuestas <- respuestas %>%
          mutate(Srvyr = NA,
                 Date = sample(x = seq(from = lubridate::today(),
                                       to = lubridate::today() + lubridate::days(3),
                                       by = "days"),
                               size = nrow(respuestas),
                               replace = T),
                 INT15 = NA,
                 T_Q = NA,
                 Duration = NA,
                 VStart = NA,
                 VEnd = NA,
                 intento_efectivo = NA)

        respuestas <- respuestas %>%
          relocate(Srvyr, Date, .before = Longitude) %>%
          relocate(SECCION, INT15, .after = Latitude)
      },
      #' @description
      #' Cálculo del error muestral de cada pregunta de opción múltiple y despliegue de resultados
      #'  en forma de objeto de ggplot2
      #' @method error_muestral_maximo Encuesta
      #' @param quitar_patron Vector tipo vacater que contiene los patrones coumnes entre nombres
      #'  de variables que se deben omitir en el cálculo.
      error_muestral_maximo = function(quitar_patron = NULL){
        aux <- self$cuestionario$diccionario %>% filter(tipo_pregunta == "multiple")
        if(!is.null(quitar_patron)) {
          quitar_patron <- paste(quitar_patron, collapse = "|")
          aux <- aux %>% filter(!grepl(quitar_patron, x = llaves))
        }

        aux <- aux %>% mutate(n = map_int(respuestas,~length(.x)))
        aux <- aux %>%
          pull(llaves) %>% map_df(~{
            # nas <- self$respuestas$base %>% summarise(any(is.na(c_across(.x)))) %>% pull(1)
            survey::svymean(survey::make.formula(.x), design = self$muestra$diseno, na.rm = T) %>%
              tibble::as_tibble(rownames = "respuesta") %>%
              # rename(SE = 3) %>%
              mutate(pregunta = .x,
                     # tiene_na = !!nas,
                     # respuesta = stringr::str_replace(string = respuesta,
                     #                                  pattern = as.character(.x),
                     #                                  replacement = "")
              ) %>% select(respuesta, mean, SE)
          }) %>% mutate(SE = qnorm(.95)*SE)

        labels <- aux %>% summarise(inf = quantile(SE,.25,na.rm = T),
                                    mid = quantile(SE,.5,na.rm = T),
                                    sup = quantile(SE,.75,na.rm = T),
                                    max = max(SE,na.rm = T)
        ) %>% mutate(iqr_min = inf-1.5*(sup-inf),
                     iqr_max = sup + 1.5*(sup-inf)) %>%
          pivot_longer(everything(), names_to = "stat", values_to = "valor")

        a <- aux %>%
          ggplot() +
          geom_boxplot(aes(x = 0, y = SE)) +
          geom_label(data = labels, aes(x = 0, y = valor, label = scales::percent(valor)),
                     hjust = 0, vjust = 0, nudge_x = .01) + labs(x = NULL) +
          scale_y_continuous(labels = scales::percent_format(1))

        b <- aux %>% filter(SE >= labels %>% filter(stat == "sup") %>% pull(valor)) %>%
          ggplot() + geom_col(aes(y = reorder(respuesta, SE), x = SE)) +
          geom_vline(xintercept = labels %>% filter(stat == "sup") %>% pull(valor))+
          labs(y = NULL)+
          scale_x_continuous(labels = scales::percent_format(1))

        print(a + b)
        return(aux)
      },
      #' @description
      #' Exportar entregables al terminar cada levantamiento.
      #' @method exportar_entregable Encuesta
      #' @param carpeta Nombre de la carpeta de destino
      #' @param agregar Vector tipo caracter que contiene las variables que se entregan en la base
      #'  de datos.
      #' @param quitar Vector tipo caracter que contiene las variables que se omiten en la base
      #'  de datos.
      exportar_entregable = function(carpeta = "entregables", agregar = NULL, quitar = NULL){

        if(!file.exists(carpeta)) dir.create(carpeta)

        # Exportar bd
        exportar_bd(self, carpeta, agregar, quitar)

        # Exportar diccionario
        self$cuestionario$diccionario %>%
          unnest(respuestas) %>%
          readr::write_excel_csv(glue::glue("{carpeta}/diccionario.csv"))

      }),
    private = list()
  )
