#' Clase Preproceso
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
#' @field patron El campo se hereda a la clase `Respuestas`.
#' @field auditoria_telefonica El campo se hereda a la clase `Respuestas`.
#' @field quitar_vars `DEPRECATED` en futuro desuso.
#' @field mantener El campo se hereda a la clase `Respuestas`.
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
Preproceso <-
  R6::R6Class(
    classname = "Preproceso",
    public = list(
      muestra = NULL,
      shp = NULL,
      cuestionario = NULL,
      bd_respuestas = NULL,
      opinometro_id = NULL,
      pool = NULL,
      bd_categorias = NULL,
      bd_correcciones = NULL,
      patron = NA,
      auditoria_telefonica = NULL,
      quitar_vars = NULL,
      mantener = NULL,
      #mantener_falta_coordenadas = NULL,
      tipo_encuesta = NULL,
      shp_completo = NULL,
      Respuestas_proc = NULL,
     # Opinometro = NULL,
    #  Resultados = NULL,
   #   Auditoria = NULL,
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
      #' @param mantener_falta_coordenadas `LOGICAL`. Determina si se descartan o no las entrevistas sin
      #'  geolocalización válida o nula.
      initialize = function(muestra = NA,
                            shp = NA,
                            cuestionario = NA,
                            bd_respuestas = NA,
                            bd_snapshot = NA,
                            opinometro_id = NA,
                            pool = NA,
                            bd_categorias = NULL,
                            bd_correcciones = NULL, # Respeuestas
                            patron = NA,
                            auditoria_telefonica = tibble(SbjNum = c(000),
                                                          razon = c("A")),
                            bd_eliminadas_reglas = NULL,
                            quitar_vars = c(),
                            mantener = "",
                           # mantener_falta_coordenadas = FALSE,
                            tipo_encuesta = "ine"
      ) {
        sf_use_s2(F)
        tipo_encuesta <- match.arg(tipo_encuesta, c("inegi","ine"))
        self$quitar_vars <- quitar_vars
        self$tipo_encuesta <- tipo_encuesta
        self$patron <- patron
       # self$mantener_falta_coordenadas <- mantener_falta_coordenadas
        self$opinometro_id <- opinometro_id
        self$pool <- pool
        self$bd_categorias <- bd_categorias


        # Valorar si no es mejor un active binding
        un <- muestra$niveles %>% filter(nivel == muestra$ultimo_nivel)
        nivel <- un %>% unite(nivel, tipo, nivel) %>% pull(nivel)
        var_n <- un %>% pull(variable)

        # # Se comprueba el metodo de obtencion de datos
        # if(("data.frame" %in% class(bd_respuestas)) & !is.na(self$opinometro_id)   ){
        #   print("No se puede cargar la base del Opinometro y una base externa, verifique el método de obtención de bases")
        #   stop()
        # }

        # Valorar active binding
        self$cuestionario <- Cuestionario$new(documento = cuestionario, patron)
        # Valorar active binding
        self$auditoria_telefonica <- auditoria_telefonica %>% distinct(SbjNum,razon, .keep_all = T)

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

        snapshot_id <- glue::glue("snapshot_id_{self$opinometro_id}")
        cluster_corregido_id <- glue::glue("cluster_corregido_id_{self$opinometro_id}")


        # eliminadas auditoria --------------------------
        bd_respuestas <- self$eliminar_auditoria_telefonica(bd_respuestas,self$auditoria_telefonica)


        # vector de ids eliminadas auditoria --------------------------
        vec_elim <- self$obtener_ids_auditoria_telefonica(bd_respuestas)

        if(!is.null(bd_eliminadas_reglas)){
        if(nrow(bd_eliminadas_reglas)>0){


        # lista de ids_con reglas -----------------------
          lista_elim_reglas <-   self$eliminar_por_regla_list(bd_respuestas,bd_eliminadas_reglas)

          # vector de ids eliminadas por regla --------------------------
          vec_elim_reglas<- unique(c(lista_elim_reglas$vec_elim_fech,lista_elim_reglas$vec_elim_usr,lista_elim_reglas$vec_elim_usr_fech))

        # eliminar por regla ---------------------------------
        }} else{
          vec_elim_reglas <- character(0)
        }

        bd_respuestas <- self$eliminar_por_reglas(bd_respuestas,vec_elim_reglas)



        # Se quedan solo las respuestas no presntes en el snapshot
        if(nrow(bd_snapshot)>0){
          bd_respuestas <- bd_respuestas |>
            anti_join(bd_snapshot,
                      by = c("Id"="SbjNum") )
        }

         # contorol_bd <- bd_respuestas |>
         #   filter(eliminada_regla==1) |>
         #   filter(eliminada_auditoria==1) |>
         #   nrow()
        # if(contorol_bd >= nrow(bd_respuestas)){
        #   print("Las bases de eliminadas contienen Ids mayores a la base de procesamiento")
        # } else{


        # Se termina el procesos si bd_respuestas está vacía
        if(nrow(bd_respuestas)>0){



        # Opinometro ---------------------------------------------------------
        if("data.frame" %in% class(bd_respuestas)) {
          opinometro <- Opinometro_proc$new(
            bd_respuestas =  bd_respuestas,
            id_cuestionarioOpinometro = self$opinometro_id,
            pool = self$pool,
            diccionario = self$cuestionario$diccionario)

          respuestas_aux <- opinometro$bd_respuestas_cuestionario

        }


        # respuestas ---------------------------------------------------------
        # Se Ejecuta la clase Respuestas  para poder hacer el procesameinto de datos
        self$Respuestas_proc <- Respuestas_proc$new(base = respuestas_aux %>% mutate(cluster_0 = SbjNum),
                                          Preproceso = self,
                                          catalogo = catalogo_variables,
                                          #mantener_falta_coordenadas = self$mantener_falta_coordenadas,
                                          muestra_completa = muestra,
                                          patron = patron,
                                          nivel = nivel,
                                          var_n = var_n)



        # TEMPORAL EN LO QUE SE AGREGA EL PROCEDIMIENTO DE ELIMANCION POR REGLA
        # -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
        # self$Respuestas_proc$base <-
        #   self$Respuestas_proc$base |>
        #   mutate(eliminada_regla = 0)
        # -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-




        # Se agregan los datos procesados al snapshot
        DBI::dbAppendTable(pool, snapshot_id, self$Respuestas_proc$base)

        # # Se carga los datos de la base de cambio de cluster
        if(!is.null(self$Respuestas_proc$cluster_corregido)){
          DBI::dbAppendTable(pool,cluster_corregido_id ,self$Respuestas_proc$cluster_corregido )
        }

        }else{
          print("No hay nuevos registros que procesar")
        }

        #}


        # Se actualiza la variable de Elimiadas

        if (length(vec_elim)>0) {
          vec_elim_str <- paste(vec_elim, collapse = ", ")

        DBI::dbExecute(pool,glue::glue("
                                       UPDATE {snapshot_id}
                                       SET eliminada_auditoria =
                                       CASE
                                        WHEN SbjNum IN ({vec_elim_str}) THEN 1
                                        ELSE 0
                                       END;
                                       ")   )
        }else {
          DBI::dbExecute(pool,glue::glue("
                                       UPDATE {snapshot_id}
                                       SET eliminada_auditoria = 0;
                                       ")   )
        }


        # Se actualiza la variable de Elimiadas por regla
        if (length(vec_elim_reglas)>0 ) {
        vec_elim_regla_str <- paste(vec_elim_reglas, collapse = ", ")

          DBI::dbExecute(pool,glue::glue("
                                       UPDATE {snapshot_id}
                                       SET eliminada_regla =
                                       CASE
                                        WHEN SbjNum IN ({vec_elim_regla_str}) THEN 1
                                        ELSE 0
                                       END;
                                       ")   )
        }else {
          DBI::dbExecute(pool,glue::glue("
                                       UPDATE {snapshot_id}
                                       SET eliminada_regla = 0;
                                       ")   )
        }





        # # Muestra (recalcula fpc)
        # self$muestra <- Muestra$new(muestra = muestra,
        #                             respuestas = self$respuestas$base,
        #                             nivel = nivel,
        #                             var_n = var_n)
        #
        #         # Informacion muestral
        #         self$respuestas$vars_diseno(muestra = self$muestra, var_n = var_n, tipo_encuesta = self$tipo_encuesta)
        #         # Diseno
        #         self$muestra$extraer_diseno(respuestas = self$respuestas$base,
        #                                     marco_muestral = self$muestra$muestra$poblacion$marco_muestral,
        #                                     tipo_encuesta = self$tipo_encuesta,
        #                                     sin_peso = self$sin_peso,
        #                                     rake = self$rake)
        #
        #         print(glue::glue("La base de campo contiene ", as.character(nrow(respuestas)), " filas"))
        #         print(glue::glue("La base de eliiminadas contiene ", as.character(nrow(self$respuestas$eliminadas)), " filas"))
        #         if(!is.null(self$respuestas$no_efectivas)  ){ print(glue::glue("La base de no efectivas contiene ", as.character(nrow(self$respuestas$no_efectivas)), " filas"))}
        #         print(glue::glue("La base de entrevistas efectivas contiene ", as.character(nrow(self$muestra$diseno$variables)), " filas"))
        #
        #         #Preguntas
        #         self$Resultados <- Resultados$new(encuesta = self, diseno = NULL, diccionario = NULL, tema = tema_morant())
        #
        #         # Shiny app de auditoria
        #         self$Auditoria <- Auditoria$new(encuesta = self, tipo_encuesta = self$tipo_encuesta)
        #         file.copy(overwrite = FALSE,
        #                   from = system.file("constantes_y_funciones/constantes.R",
        #                                      package = "encuestar",
        #                                      mustWork = TRUE),
        #                   to = "R")
        #         file.copy(overwrite = FALSE,
        #                   from = system.file("constantes_y_funciones/funciones.R",
        #                                      package = "encuestar",
        #                                      mustWork = TRUE),
        #                   to = "R")
        #         source(file = paste0(getwd(), "/R/constantes.R"))
        #beepr::beep()

      },
      #' @description Descarta las entrevistas que, de acuerdo a la base de auditoria telefónica,
      #'  deben ser descartadas.
      #' @param auditoria_telefonica [tibble()] que contiene las entrevistas que, de acuerdo a
      #'  auditoría, se van a eliminar del registro.
      eliminar_auditoria_telefonica = function(base,auditoria_telefonica){

        if(("Id" %in% names(base)) &
           ("SbjNum" %in% names(auditoria_telefonica))){
          if(is.character(auditoria_telefonica$SbjNum)) auditoria_telefonica <- auditoria_telefonica %>% mutate(SbjNum = readr::parse_double(SbjNum))
          # Se eliminan por no pasar la auditoria telefonica
          n <- nrow(base)

          #self$eliminadas <- base %>% inner_join(auditoria_telefonica, by = "SbjNum")

          base <- base %>%
            #select(SbjNum) |>
            mutate(eliminada_auditoria = ifelse(Id %in% (auditoria_telefonica |> distinct(SbjNum) |> pull()),1,0))
            # anti_join(auditoria_telefonica, by="SbjNum")
        }
        else cat("Identificador SbjNum no presente en alguna de las bases para eliminar por auditoria telefonica")
        return(base)

      },
      #' @description Obtener los Id´s de las filas eliminadas
      #' @param bd_respuestas que contiene las entrevistas con la v ariable 'eliminada_auditoria'
      obtener_ids_auditoria_telefonica = function(bd_respuestas){

          vec_elim_sal <- bd_respuestas %>%
            select(Id,eliminada_auditoria) |>
            filter(eliminada_auditoria == 1) |>
            distinct(Id) |>
            pull()

          return(vec_elim_sal)

      },
   #' @description Genera la lista de los Id que por regla serán eliminadas
   #' @param base Contiene la base de respuestas
   #' @param bd_eliminadas_reglas Contiene la base de reglas de eliminacion
   eliminar_por_regla_list = function(base,bd_eliminadas_reglas){

     bd_eliminadas_reglas <-
       bd_eliminadas_reglas |>
       mutate(razon =  case_when(
         is.na(encuestador) & !is.na(fecha_inicio) & !is.na(fecha_fin) ~ "Eliminado por fecha",
         !is.na(encuestador) & is.na(fecha_inicio) & is.na(fecha_fin) ~ "Eliminado por usuario",
         !is.na(encuestador) & !is.na(fecha_inicio) & !is.na(fecha_fin) ~ "Eliminado por usuario y fecha",
         .default = NA
       )  )

     vec_elim_fech <- c()
     vec_elim_usr <- c()
     vec_elim_usr_fech <- c()

     # Filtro por fecha
     n_fecha <- bd_eliminadas_reglas |>
       filter(razon == "Eliminado por fecha") |>
       nrow()

     bd_regla_fech <- bd_eliminadas_reglas |>
       filter(razon == "Eliminado por fecha") |>
       select(fecha_inicio,fecha_fin) # |>
     #  mutate(
     #    fecha_inicio = lubridate::ymd_hm(fecha_inicio),
     #    fecha_fin    = lubridate::ymd_hm(fecha_fin)
     #  )

     for(fech in seq_len(n_fecha)){

       vec_elim_fech_aux <-  base |>
         filter(bd_regla_fech$fecha_inicio[fech] <= FechaInicio &  bd_regla_fech$fecha_fin[fech] >= FechaInicio ) |>
         pull(Id)

       vec_elim_fech <- c(vec_elim_fech, vec_elim_fech_aux)

     }

     # Filtro por usuario
     usuarios_elim <- bd_eliminadas_reglas |>
       filter(razon == "Eliminado por usuario") |>
       pull(encuestador)


     vec_elim_usr <- base |>
       mutate(nombre_usr = paste(Nombre, APaterno, AMaterno, sep = " ")) |>
       filter(nombre_usr %in%  usuarios_elim) |>
       pull(Id)

     # Filtro por usuario y fecha
     n_fecha_usr <- bd_eliminadas_reglas |>
       filter(razon == "Eliminado por usuario y fecha") |>
       nrow()

     bd_regla_fech_usr <- bd_eliminadas_reglas |>
       filter(razon == "Eliminado por usuario y fecha") |>
       select(fecha_inicio,fecha_fin,encuestador) # |>
     #  mutate(
     #    fecha_inicio = lubridate::ymd_hm(fecha_inicio),
     #    fecha_fin    = lubridate::ymd_hm(fecha_fin)
     #  )

     for(fech_usr in seq_len(n_fecha_usr)){

       vec_elim_fech_aux <-  base |>
         mutate(nombre_usr = paste(Nombre, APaterno, AMaterno, sep = " ")) |>
         filter(nombre_usr %in%  bd_regla_fech_usr$encuestador[fech_usr]) |>
         filter(bd_regla_fech_usr$fecha_inicio[fech] <= FechaInicio &  bd_regla_fech_usr$fecha_fin[fech] >= FechaInicio ) |>
         pull(Id)

       vec_elim_usr_fech <- c(vec_elim_usr_fech, vec_elim_fech_aux)

     }

     # Empaquetamos ambos resultados en una lista
     return(list(
       vec_elim_fech     = vec_elim_fech,
       vec_elim_usr = vec_elim_usr,
       vec_elim_usr_fech = vec_elim_usr_fech
     ))
   },
   #' @description Descarta las entrevistas que, de acuerdo al vector de ,
   #'  deben ser descartadas.
   #' @param auditoria_telefonica [tibble()] que contiene las entrevistas que, de acuerdo a
   #'  auditoría, se van a eliminar del registro.
   eliminar_por_reglas = function(base,vector_reglas){

     if(length(vector_reglas)>0){
       #self$eliminadas <- base %>% inner_join(auditoria_telefonica, by = "SbjNum")

       base <- base %>%
         #select(SbjNum) |>
         mutate(eliminada_regla = ifelse(Id %in% vector_reglas ,1,0))
       # anti_join(auditoria_telefonica, by="SbjNum")
     }
     else {
       print(" No hay reglas de eliminacion que considerar")
     }
     return(base)

   }
    ),
    private = list()
  )
