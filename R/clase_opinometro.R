#' Clase Opinómetro
#'
#' @description La clase `Opinómetro` gestiona las rutinas necesarias para determinar el diccionario
#'  de procesamiento y la base de datos de respuestas en campo. La clase `Opinómetro` es de
#'  importante relevancia pues brinda a la paquetería una fuente de datos e insumos distinta a la que
#'  históricamente se a usado que es `SurveyToGo`.
#'
#' @field id_cuestionarioOpinometro Campo heredado de la clase `Encuesta`.
#' @field pool Campo heredado de la clase `Encuesta`.
#' @field variables_cuestionario Nombres de las variables que, de acuerdo al diccionario, forman el
#'  conjunto de preguntas de la encuesta
#' @field bd_respuestas_cuestionario [tibble()] que contiene las respuestas serán utilizadas para
#'  construir el diseño muestral de la encuesta.
#' @field diccionario Diccionario (o codebook) del cuestionario de la encuesta. Heredado de la
#'  clase `Encuesta` o se provee de forma externa.
Opinometro <-
  R6::R6Class(
    classname = "Opinometro",
    public = list(
      id_cuestionarioOpinometro = NULL,
      pool = NULL,
      contenido_cuestionario = NULL,
      variables_cuestionario = NULL,
      bd_respuestas_raw = NULL,
      bd_respuestas_cuestionario = NULL,
      diccionario = NULL,
      #' @description Establece conexión con la base de datos donde está alojada la
      #'  implementación del `Opinómetro`.
      #' @param id_cuestionarioOpinometro Valor entero. Identificador del cuestionario
      #'  aplicado en campo generado en la plataforma `Opinómetro`.
      #' @param pool Objeto tipo [pool] generado al conectarse a la base de datos que
      #'  almacena las respuestas. Se recomienda utilizar la función [dbPool()] de la
      #'  paquetería [pool] para esto.
      #' @param diccionario [tibble()] Diccionario (o codebook) del cuestionario de la
      #'  encuesta. Aunque la clase `Opinometro` reproduce el diccionario a partir del
      #'  cuestionario construido en la plataforma `Opinometro`, el parametro `diccionario`
      #'  es aquel que históricamente se ha utilizado para produccion. Es decir, aquel que
      #'  está usualmente guardado en un archivo de excel y/o que recibe la clase `Encuesta`
      #'  en el campo `diccionario`.
      initialize = function(
        id_cuestionarioOpinometro = NA,
        pool = NA,
        diccionario = NULL
      ) {
        self$pool <- pool
        self$id_cuestionarioOpinometro <- id_cuestionarioOpinometro
        self$diccionario <- diccionario
        # self$definir_contenido()
        # self$construir_respuestas()
        # if(!is.null(self$diccionario)) {
        #   self$verificar_variables()
        # }
        # self$terminar_conexion()
      },
      #' @description Determina las variables que están contenidas en la plataforma
      #'  `Opinometro`a partir de los campos construidos en el cuestinoario.
      definir_contenido = function() {
        self$contenido_cuestionario <-
          determinar_contenidoCuestionario(
            pool = self$pool,
            id_cuestionario = self$id_cuestionarioOpinometro
          )

        self$variables_cuestionario <-
          self$contenido_cuestionario |>
          pull(elementsname)
      },
      #' @description Construye el [tibble()] de respuestas contenidas en la base de datos
      #'  que tiene implementada la plataforma ``Opinometro`.
      construir_respuestas = function() {
        self$bd_respuestas_raw <-
          consultar_respuestas_existentes(
            pool = self$pool,
            id_cuestionario = self$id_cuestionarioOpinometro
          )

        # Se asume que si es encuesta, existe un diccinoario
        if (!is.null(self$diccionario)) {
          variables_sobrantes <-
            self$bd_respuestas_raw |>
            colnames() |>
            as_tibble() |>
            filter(
              !value %in%
                c(
                  "Id",
                  "EncuestaId",
                  "FechaInicio",
                  "FechaFin",
                  "FechaCreada",
                  "UbicacionAplicada",
                  "UsuarioNum",
                  "Nombre",
                  "APaterno",
                  "AMaterno",
                  "TipoRegistro"
                )
            ) |>
            anti_join(
              self$diccionario |>
                pull(llaves) |>
                as_tibble(),
              by = "value"
            )

          variables_faltantes <-
            self$diccionario |>
            pull(llaves) |>
            as_tibble() |>
            anti_join(
              self$bd_respuestas_raw |>
                colnames() |>
                as_tibble(),
              by = "value"
            )

          if (nrow(variables_sobrantes) != 0) {
            print(glue::glue(
              "Las siguientes variables están en el opinometro pero no estan en el diccionario: "
            ))
            print(
              variables_sobrantes |>
                rename(variable = value)
            )
          }

          if (nrow(variables_faltantes) != 0) {
            print(glue::glue(
              "Las siguientes variables están en el diccionario pero no existen en los registros del opinometro:"
            ))
            print(
              variables_faltantes |>
                rename(variable = value)
            )

            crear_variables <-
              yesno::yesno2(
                "Se van a crear las variables faltantes a la base consultada. Presione 'Y' para continuar",
                yes = "Y"
              )
            if (crear_variables) {
              for (i in variables_faltantes$value) {
                self$bd_respuestas_cuestionario <-
                  self$bd_respuestas_raw |>
                  mutate(!!rlang::sym(i) := NA_character_)
              }
            }
          }

          var_faltantes_opin <-
            self$variables_cuestionario |>
            as_tibble() |>
            anti_join(
              self$bd_respuestas_raw |>
                colnames() |>
                as_tibble() |>
                filter(
                  !value %in%
                    c(
                      "Id",
                      "EncuestaId",
                      "FechaInicio",
                      "FechaFin",
                      "FechaCreada",
                      "UbicacionAplicada",
                      "UsuarioNum",
                      "Nombre",
                      "APaterno",
                      "AMaterno",
                      "TipoRegistro"
                    )
                ),
              by = "value"
            )

          if (nrow(var_faltantes_opin) != 0) {
            print(glue::glue(
              "Las siguientes variables están en el registro del opinometro pero no estan en la base del opinómetro: "
            ))
            print(
              var_faltantes_opin |>
                rename(variable = value)
            )
          }

          variables_opinometro_efect <- self$variables_cuestionario[
            !self$variables_cuestionario %in% pull(var_faltantes_opin)
          ]

          self$bd_respuestas_cuestionario <-
            self$bd_respuestas_raw |>
            rectificar_respuestasOpinometro(
              variables_cuestionario = variables_opinometro_efect
            ) |>
            left_join(
              self$bd_respuestas_raw |>
                calcular_intentosEfectivos_opinometro(),
              by = "SbjNum"
            )
        }
      },
      #' @description Notifica al usuario si hay variables faltantes o sobrantes en el [tibble]
      #'  que contiene las respuestas.
      verificar_variables = function() {
        variables_sobrantes <-
          self$bd_respuestas_cuestionario |>
          select(all_of(self$variables_cuestionario)) |>
          colnames() |>
          as_tibble() |>
          anti_join(
            self$diccionario |>
              pull(llaves) |>
              as_tibble(),
            by = "value"
          )

        variables_faltantes <-
          self$diccionario |>
          pull(llaves) |>
          as_tibble() |>
          anti_join(
            self$bd_respuestas_cuestionario |>
              select(all_of(self$variables_cuestionario)) |>
              colnames() |>
              as_tibble(),
            by = "value"
          )

        if (nrow(variables_sobrantes) != 0) {
          print(glue::glue(
            "Las siguientes variables están en el opinometro pero no estan en el diccionario: "
          ))
          print(
            variables_sobrantes |>
              rename(variable = value)
          )
        }

        if (nrow(variables_faltantes) != 0) {
          print(glue::glue(
            "Las siguientes variables están en el diccionario pero no existen en los registros del opinometro:"
          ))
          print(
            variables_faltantes |>
              rename(variable = value)
          )

          crear_variables <-
            yesno::yesno2(
              "¿Crear las variables faltantes a la base consultada?",
              yes = "Y",
              no = "N"
            )
          if (crear_variables) {
            for (i in variables_faltantes$value) {
              self$bd_respuestas_cuestionario <-
                self$bd_respuestas_cuestionario |>
                mutate(!!rlang::sym(i) := NA_character_)
            }
          }
        }
      }
      # terminar_conexion = function(){
      #   pool::poolClose(pool = self$pool)
      #
      # }
    )
  )
