#' Clasoe Respuestas
#'
#' @description La clase `Respuestas` tiene como objetivo coonsolidar, limpiar y preparar las
#'  respuestas de los individuos entrevistados en campo.
#'
#' @field eliminadas El campo `eliminadas` contiene el [tibble()] de entrevistas eliminadas por
#'  auditoría.
#' @field cluster_corregido El campo `cluster_corregido` contiene el [tibble()] de entrevistas cuyo
#'  cluster haya sido corregido por geolocalización.
#' @field base El campo `base` contiene el [tibble()] de las respuestas serán utilizadas para construir
#'  el diseño muestral de la encuesta.
#' @field catalogo El campo `catálogo` contiene el catálogo de variables en el cual se definen aqueelas
#'  que son necesarias y suficientes para construir un diseño muestral.
#' @field n El campo `n` está en desuso.
#' @field m El campo `m` está en desuso.
#' @field sin_coordenadas El campo `sin_coordenadas` contiene el [tibble()] de, en caso de existir,
#'  entrevistas que no tienen una geolicalización válida.
#' @field mantener_falta_coordenadas `LOGICAL` determina si las entrevistas que no tienen una
#'  geolocalización válida se descartan o no.
Respuestas <-
  R6::R6Class(
    classname = "Respuestas",
    inherit = Encuesta,
    public = list(
      eliminadas = NULL,
      no_efectivas = NULL,
      cluster_corregido = NULL,
      base = NULL,
      catalogo = NULL,
      n = NULL,
      m = NULL,
      sin_coordenadas = NULL,
      mantener_falta_coordenadas = NULL,
      #' @description Se reciben los diferentes insumos relacionados a la encuesta para estandarizar
      #'  las respuestas que formarán parte del diseÑo muestral.
      #' @param base [tibble()] que contiene la base de datos de respuestas de las personas entrevistadas.
      #' @param catalogo [tibble()] que contiene el catalogo de variables.
      #' @param encuesta Clase de jerarquía mayor.
      #' @param muestra_completa Objeto de formato personalizado generado por la paquetería `muestrear`.
      #'  Contiene el diseño muestral completo de la encuesta.
      #' @param mantener_falta_coordenadas `LOGICAL` determina si las entrevistas que no tienen una
      #'  geolocalización válida se descartan o no.
      #' @param patron Cadena de caracteres que se removerán de todas las respuestas recibidas en
      #'  campo.
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro.
      #' @param var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      initialize = function(base = NULL,
                            catalogo = NA,
                            encuesta = NULL,
                            muestra_completa = NULL,
                            mantener_falta_coordenadas = FALSE,
                            patron = NA,
                            nivel = NULL,
                            var_n = NULL) {
        shp <- encuesta$shp
        mantener <- encuesta$mantener
        diccionario <- encuesta$cuestionario$diccionario

        if(!identical(names(encuesta$auditoria_telefonica), c("SbjNum", "razon"))) stop("Los nombres de las columnas de la base de datos de auditoría telefónica deben ser: 'SbjNum, razon'")

        if(!is.null(encuesta$bd_correcciones)){
          if(!identical(names(encuesta$bd_correcciones), c("SbjNum", "llave", "capturada", "correccion"))) stop("Los nombres de las columnas de la base de datos de correcciones deben ser: 'SbjNum, llave, capturada, correccion'")
        }

        auditoria_telefonica <- encuesta$auditoria_telefonica
        bd_correcciones <- encuesta$bd_correcciones
        muestra <- muestra_completa$muestra %>% purrr::pluck(var_n)

        self$base <- base
        self$catalogo <- catalogo

        # Parar si faltan variables de sistema o del cuestionario
        # self$nombres(catalogo_variables = self$catalogo,
        #              bd =  self$base,
        #              diccionario = diccionario)

        # Quitar patrones a respuestas
        self$q_patron(self$base, diccionario, patron)

        # Parar si opciones de respuesta no coinciden con diccionario
        self$categorias(self$base, diccionario)

        # Advertir si hay opciones de respuestas que tienen count = 0
        self$respuestas_sin_seleccion(bd = self$base, diccionario)

        # Limpiar las que no pasan auditoria telefonica
        self$eliminar_auditoria_telefonica(auditoria_telefonica)

        # Corregir respuestas registradas mal por los encuestadores
        if(!is.null(bd_correcciones)) {

          self$base <- self$corregir_respuestas(respuestas = self$base, bd_correcciones_raw = bd_correcciones)

        }

        self$crear_variablesSecundarias(diccionario = diccionario)

        # Mantener respuestas que no tienen coordenadas
        if(mantener_falta_coordenadas){

          self$coordenadas_faltantes()

        } else {

          # Limpiar las respuestas que no tienen coordenadas
          self$eliminar_falta_coordenadas()

        }

        # Eliminar entrevistas cuyo cluster no pertenece a la muestra
        self$eliminar_fuera_muestra(self$base, muestra, nivel, var_n)

        # Se pregunta si existe la pregunta de resouestas no efectivas
        if( TRUE %in% (names(self$base) %in% c("TipoRegistro"))  ){

        # Eliminar entrevistas que no son efectivas
        self$retirar_no_efectivas(self$base)

        }else{
          print("No se existe la variable/columna 'TipoRegistro' para identificar entrevistas efectivas")
        }

        # Corregir cluster equivocado
        # browser()
        # self$correccion_cluster(self$base, shp, mantener, nivel, var_n)

        # Calcular distancia de la entrevista al cluster correcto
        # self$calcular_distancia(base = self$base,
        #                         encuesta = encuesta,
        #                         muestra = muestra_completa,
        #                         var_n = var_n, nivel = nivel)

        # Limpiar las que no tienen variables de diseno
        # self$eliminar_faltantes_diseno() # no entiendo por qué
        if(mantener_falta_coordenadas){
          self$base <- self$base %>% bind_rows(
            self$sin_coordenadas
          )
        }

        # Cambiar variables a tipo numerica
        numericas <- diccionario %>% filter(tipo_pregunta == "numericas") %>%
          pull(llaves)

        self$base <- self$base %>%
          mutate(across(all_of(numericas), ~readr::parse_number(.x)))

        # self$eliminadas <- anti_join(base, self$base, by = "SbjNum")
      },
      #' @description Determina si la base de respuestas contiene todas las variables que, de acuerdo
      #'  al diccionario, deben ser procesadas.
      #' @param catalogo_variables [tibble()] que contiene el catálogo de variables.
      #' @param bd [tibble()] que contiene las respuestas recolectadas en campo
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      nombres = function(catalogo_variables, bd, diccionario){

        faltantes <-
          catalogo_variables |>
          filter(segundo_nivel %in% c("sistema", "cuestionario")) |>
          filter(!variable %in% c("id", "fecha_inicio", "ubicacion_aplicada")) |>
          anti_join(tibble(variable = names(bd)),
                    by = 'variable')

        # faltantes <- is.na(match(diccionario$llaves, names(bd)))
        if(nrow(faltantes) > 0){
          stop(glue::glue("Las siguientes variables no se encuentran en la base de datos: {paste(faltantes$variable, collapse = ', ')}"))
        }

      },
      #' @description Quita cadenas de texto de las respuetas de campo heredadas del cuestionario
      #' @param bd [tibble()] que contiene las respuestas recolectadas en campo
      #' @param dicc Diccionario (o codebook) del cuestionario de la encuesta.
      #' @param patron Cadena o cadenas de texto que se desean quitar de las respuestas de campo
      q_patron = function(bd, dicc, patron){
        if(!is.na(patron)){
          aux <- bd %>%
            mutate(across(c(all_of(dicc$llaves),where(is.character)),
                          ~stringr::str_squish(gsub(x = .x,
                                                    pattern = patron,
                                                    ""))))
          self$base <- aux
        }
      },
      #' @description Determina diferencias entre las respuestas recolectadas en campo y las esperadas
      #'  de acuerdo al diccionario
      #' @param bd [tibble()] que contiene las respuestas recolectadas en campo
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      categorias = function(bd, diccionario){
        discrepancia <-
          diccionario %>%
          filter(tipo_pregunta != "Abierta") |>
          pull(llaves) %>%
          map_df(~{
            res <-
              bd %>%
              count(across(all_of(.x))) %>%
              na.omit %>%
              pull(1)

            m <- match(res,
                       diccionario %>%
                         filter(llaves == .x) %>%
                         mutate(respuestas = stringr::str_split(string = respuestas, pattern = "_")) |>
                         pull(respuestas) |>
                         pluck(1)
            )

            tibble(llave = .x,
                   sin_respuestas = res[is.na(m)]
            )

          })
        if(nrow(discrepancia)>0){

          print(glue::glue("La siguiente tabla muestra las respuestas en la base de campo que no están contempladas en el diccionario"))
          print(discrepancia |>
                  rename(codigo_pregunta = llave,
                         respuesta_campo = sin_respuestas),
                n = Inf)
          print(glue::glue("Revise las respuestas entre la base de campo y el cuestionario de procesamiento y corrija. Estas discrepancias no deberían existir."),
                immediate. = T)

        }
      },
      #' @description Determina las respuestas esperadas que no han sido seleccionadas por los
      #'  entrevistados
      #' @param bd [tibble()] que contiene las respuestas recolectadas en campo
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      respuestas_sin_seleccion = function(bd, diccionario){

        bd_sinregistros <-
          diccionario |>
          filter(tipo_pregunta != "Abierta") |>
          pull(llaves) %>%
          purrr::map_df(.x = .,
                        .f = ~ {
                          respuestas_sin_registros <-
                            diccionario %>%
                            filter(llaves == .x) %>%
                            mutate(respuestas = stringr::str_split(string = respuestas, pattern = "_")) |>
                            pull(respuestas) |>
                            pluck(1) |>
                            as_tibble() |>
                            anti_join(bd |>
                                        count(across(all_of(.x))) %>%
                                        arrange(1) |>
                                        pull(1) |>
                                        as_tibble(),
                                      by = "value") |>
                            pull()

                          tibble(codigo_pregunta = .x,
                                 respuesta_sin_registros = respuestas_sin_registros)

                        })

        if(nrow(bd_sinregistros) > 0) {

          warning(paste("La siguiente tabla muestra las respuestas que tienen cero registros en la base de respuestas", sep = ""),
                  immediate. = T)
          print(bd_sinregistros, n = Inf)

        }

      },
      #' @description Descarta las entrevistas que, de acuerdo a la base de auditoria telefónica,
      #'  deben ser descartadas.
      #' @param auditoria_telefonica [tibble()] que contiene las entrevistas que, de acuerdo a
      #'  auditoría, se van a eliminar del registro.
      eliminar_auditoria_telefonica = function(auditoria_telefonica){

        if(("SbjNum" %in% names(self$base)) &
           ("SbjNum" %in% names(auditoria_telefonica))){
          #if(is.character(auditoria_telefonica$SbjNum)) auditoria_telefonica <- auditoria_telefonica %>% mutate(SbjNum = readr::parse_double(SbjNum))
          # Se eliminan por no pasar la auditoria telefonica
          n <- nrow(self$base)

          self$eliminadas <- self$base %>% inner_join(auditoria_telefonica, by = "SbjNum")

          self$base <- self$base %>%
            anti_join(auditoria_telefonica, by="SbjNum")

          # Mandar mensaje
          print(
            glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por auditoria telefonica")
          )
        }
        else cat("Identificador SbjNum no presente en alguna de las bases para eliminar por auditoria telefonica")
        return(self$base)

      },
      #' @description Corrige las respuestas de los entrevistados de acuerdo a la base de correcciones
      #' @param respuestas [tibble()] que contiene las respuestas recolectadas en campo
      #' @param bd_correcciones_raw [tibble()] que contiene la relación de entrevista, variable en la
      #'  cual está incorrecto el registro y valor correcto de la respuesta
      corregir_respuestas = function(respuestas, bd_correcciones_raw){

        variables_corregidas <-
          bd_correcciones_raw |>
          distinct(llave) |>
          pull()

        bd_correcciones <-
          bd_correcciones_raw |>
          select(!capturada) |>
          mutate(llave = paste0(llave, "_correccion")) |>
          tidyr::pivot_wider(id_cols = SbjNum,
                             names_from = llave,
                             values_from = correccion)

        respuestas <-
          respuestas |>
          left_join(bd_correcciones, by = "SbjNum") %>%
          mutate(across(.cols = all_of(variables_corregidas),
                        .fns = ~ dplyr::if_else(condition = !is.na(get(paste0(cur_column(), "_correccion"))),
                                                true = get(paste0(cur_column(), "_correccion")),
                                                false = .))) |>
          select(!all_of(names(bd_correcciones)[-1]))

        return(respuestas)
      },
      #' @description Crea variables que, por defecto, se usan en la producción y entrega de resultados
      #'  de Morant Consultores
      #' @param diccionario Diccionario (o codebook) del cuestionario de la encuesta.
      crear_variablesSecundarias = function(diccionario){
        self$base <-
          self$base |>
          dplyr::mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                               edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                               edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                               edad >= 56  ~ "Baby Boomers (56 años o más)"),
                        generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                                   "Millenials (26 a 40 años)",
                                                                   "Generación X (41 a 55 años)",
                                                                   "Baby Boomers (56 años o más)")))
          # dplyr::mutate(amai_jefegrado = case_when(jefe_grado %in% c("No estudió", "No contesta") ~ 0,
          #                                          jefe_grado == "Primaria incompleta" ~ 6,
          #                                          jefe_grado == "Primaria completa" ~ 11,
          #                                          jefe_grado == "Secundaria incompleta" ~ 12,
          #                                          jefe_grado == "Secundaria completa" ~ 18,
          #                                          jefe_grado == "Preparatoria incompleta" ~ 23,
          #                                          jefe_grado == "Preparatoria completa" ~ 27,
          #                                          jefe_grado == "Licenciatura incompleta" ~ 36,
          #                                          jefe_grado == "Licenciatura completa" ~ 59,
          #                                          jefe_grado == "Diplomado o maestría" ~ 85,
          #                                          jefe_grado == "Diplomado o maestría" ~ 85,
          #                                          jefe_grado == "Doctorado" ~ 85, .default = NA),
          #               #+ Se crean las variables para definir nivel socieconómico segun el estandar AMAI 2022
          #               #+ Revisar: https://amai.org/descargas/Nota_Metodologico_NSE_2022_v5.pdf
          #               amai_cantidadwc = case_when(cantidad_wc == "0" ~ 0,
          #                                           cantidad_wc == "1" ~ 24,
          #                                           cantidad_wc == "2 o más" ~ 47,
          #                                           .default = NA),
          #               amai_cantidadautos = case_when(cantidad_autos == "0" ~ 0,
          #                                              cantidad_autos == "1" ~ 22,
          #                                              cantidad_autos == "2 o más" ~ 43,
          #                                              .default = NA),
          #               amai_internet=case_when(internet == "No tiene" ~ 0,
          #                                       internet == "Sí tiene" ~ 32,
          #                                       .default = NA),
          #               amai_trabajo=case_when(trabajo == "0" ~ 0,
          #                                      trabajo == "1" ~ 15,
          #                                      trabajo == "2" ~ 31,
          #                                      trabajo == "3" ~ 46,
          #                                      trabajo == "4 o más" ~ 61,
          #                                      .default = NA),
          #               amai_cantidadcuartos = case_when(cantidad_cuartos == "0" ~ 0,
          #                                                cantidad_cuartos == "1" ~ 8,
          #                                                cantidad_cuartos == "2" ~ 16,
          #                                                cantidad_cuartos == "3" ~ 24,
          #                                                cantidad_cuartos == "4 o más" ~ 32,
          #                                                .default = NA)) %>%
          # dplyr::mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
          #               nivel_socioec = case_when(
          #                 (suma_amai >= 0 & suma_amai <= 47) ~ "E",
          #                 (suma_amai >= 48 & suma_amai <= 94) ~ "D",
          #                 (suma_amai >= 95 & suma_amai <= 115) ~ "D_mas",
          #                 (suma_amai >= 116 & suma_amai <= 140) ~ "C_menos",
          #                 (suma_amai >= 141 & suma_amai <= 167) ~ "C",
          #                 (suma_amai >= 168 & suma_amai <= 201) ~ "C_mas",
          #                 suma_amai >= 202 ~ "A_B",
          #                 .default = NA),
          #               nivel_socioec = factor(x = nivel_socioec,
          #                                      levels = c("E",
          #                                                 "D",
          #                                                 "D_mas",
          #                                                 "C_menos",
          #                                                 "C",
          #                                                 "C_mas",
          #                                                 "A_B")))

      },
      #' @description Determina las entrevistas con variables de geolocalización no válidas
      coordenadas_faltantes = function(){
        self$sin_coordenadas <- self$base %>% filter(is.na(Longitude) | is.na(Latitude))
        self$base <- self$base %>% filter(!is.na(Longitude) | !is.na(Latitude))
      },
      #' @description Descarta y registra las entrevistas con variables de geolocalización no válidas
      #'  y comunica al usuario esa operación
      eliminar_falta_coordenadas = function(){

        if("Longitude" %in% names(self$base) & "Latitude" %in% names(self$base)){
          n <- nrow(self$base)
          self$base <- self$base %>% filter(!is.na(as.numeric(Longitude)) & !is.na(as.numeric(Latitude)))
          self$eliminadas <- self$eliminadas %>% bind_rows(
            self$base %>% filter(!(!is.na(as.numeric(Longitude)) & !is.na(as.numeric(Latitude)))) %>% mutate(razon = "Sin coordenadas")
          )
          print(
            glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por falta de coordenadas")
          )
        } else{
          print("No existe la columna Longitude ni Latitude en la base de respuestas")
        }
      },
      #' @description Con base en la geolocalización de la entrevista, determina el cluster (o sección)
      #'  más cercano y le asigna el número de ese cluster. Para el diseno muestral esto implica
      #'  que la entrevista fue levantada en el cluster más cercano a la geolocalización.
      #' @param base [tibble()] que contiene las respuestas recolectadas en campo
      #' @param shp Campo de la clase encuesta. contiene toda la información cartográfica de la
      #'  encuesta
      #' @param mantener Campo de la clase encuesta. Las entrevistas mantendrán el registro del
      #'  cluster en el cual hayan sido levantadas sin importas si hay otro cluster más cercano.
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro
      #' @param var_n var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      correccion_cluster = function(base, shp, mantener, nivel, var_n){
        aux <- corregir_cluster(base, shp, mantener, nivel, var_n)

        self$cluster_corregido <- self$base %>%
          select(all_of(c("SbjNum", "Srvyr", "Date", "Longitude", "Latitude", var_n))) %>%
          anti_join(aux, by = c("SbjNum", var_n)) %>%
          left_join(aux %>% select(all_of(c("SbjNum", var_n))), by = "SbjNum") %>%
          rename(anterior = 6, nueva = 7)

        self$base <- aux
      },
      #' @description Descarta las entrevistas cuyo cuyo cluster levantado no pertenezca a la lista
      #'  de clusters en muestra y notifica al usuario.
      #' @param respuestas [tibble()] que contiene las respuestas recolectadas en campo
      #' @param muestra Contiene el diseño muestral completo de la encuesta.
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro
      #' @param var_n var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      eliminar_fuera_muestra = function(respuestas, muestra, nivel, var_n){

        self$base <- respuestas %>%
          semi_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                    by = set_names(nivel,var_n))

        self$eliminadas <- self$eliminadas %>% bind_rows(
          respuestas %>%
            anti_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                      by = set_names(nivel,var_n)) %>%
            mutate(razon = "Cluster no existente")
        )
        print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que el cluster no pertenece a la muestra"))
      },
      #' @description Calcula la distancia (en metros) entre la geolocalización donde se lvantó la
      #'  entrevista y el cluster más cercano que encuentre.
      #' @param base [tibble()] que contiene las respuestas recolectadas en campo
      #' @param encuesta Clase Encuesta de jerarquía mayor
      #' @param muestra Contiene el diseño muestral completo de la encuesta.
      #' @param nivel Valor tipo entero que indica el número de etapas de muestro
      #' @param var_n var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      calcular_distancia = function(base, encuesta, muestra, var_n, nivel){

        aux_sf <- base %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

        pol <- dist_poligonos(aux_sf, shp = encuesta$shp, var_n, nivel)
        puntos <- dist_puntos(aux_sf, encuesta, muestra, var_n, nivel)

        if(nrow(puntos) > 0){
          respuestas <- pol %>%
            anti_join(
              puntos %>% as_tibble, by = "SbjNum"
            ) %>% bind_rows(
              puntos
            )
        } else {
          respuestas <- pol
        }

        self$base <- respuestas %>% left_join(base %>% select(SbjNum, Longitude, Latitude), by = "SbjNum")
      },
      #' @description Descartar entrevistas que por alguna razón u otra no se calculó la información
      #'  relacionada a su diseño muestral
      eliminar_faltantes_diseno = function(){
        n <- nrow(self$base)

        self$base <- self$base %>%
          filter(
            across(
              .cols = c(starts_with("id_"),
                        starts_with("fpc_"),
                        starts_with("prob_"),
                        starts_with("pob_")),
              .fns = ~ !is.na(.x)
            ))
        print(
          glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por tener datos faltantes en las variables de diseno muestral")
        )
        return(self$base)
      },
      #' @description Agrega las variables relacionadas al diseno muestral
      #' @param muestra Contiene el diseño muestral completo de la encuesta.
      #' @param var_n var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      #' @param tipo_encuesta Campo de la clase Encuesta que determina el tipo de encuesta levantada.
      vars_diseno = function(muestra, var_n, tipo_encuesta){
        vars_join <- c(var_n,
                       names(muestra$base)[is.na(match(names(muestra$base), names(self$base)))]
        )
        self$base <- self$base %>%
          inner_join(muestra$base %>% select(all_of(vars_join)))

        if(tipo_encuesta == "inegi"){
          self$base <- self$base %>%
            mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                                 c("18A24","25A59","60YMAS"))),
                   sexo = if_else(sexo == "Mujer", "F", "M"))
        }

        if(tipo_encuesta == "ine"){
          self$base <- self$base %>%
            mutate(rango_edad = cut(as.numeric(edad), c(17,24,39,59,Inf),
                                    labels = c("18A24","25A39","40A59","60YMAS")),
                   sexo = if_else(sexo == "Mujer", "F", "M"))
        }

        if(sum(grepl("region", muestra$muestra$niveles$variable)) > 0){
          var_reg <- muestra$muestra$niveles %>% filter(variable == "region") %>%
            unite("var_reg", c(tipo, nivel)) %>% pull(var_reg)
          self$base <- self$base %>%
            inner_join(muestra$muestra$poblacion$marco_muestral %>%
                         distinct(across(all_of(var_reg)), region), by = var_reg)
        }
      },
      #' @description Elimina las filas que no son efectivas
      #' @param respuestas Contiene la base de respuestas
      retirar_no_efectivas = function(respuestas){



        if(TRUE %in% ((distinct(respuestas,TipoRegistro) |> pull(TipoRegistro)) %in% c("Efectivo"))){

        self$base <- respuestas %>%
         filter(TipoRegistro == "Efectivo")

        self$no_efectivas <-
          respuestas %>%
            filter(TipoRegistro != "Efectivo") %>%
            mutate(razon = "No efectivas")

        print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que no son efectivas"))

        } else{
          print("Se necesita que la variable/columna 'TipoRegistro' tenga como mínimo el valor 'Efectivo' para identificar entrevistas efectivas")
        }
      }
    )
  )
