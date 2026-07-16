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
      pool = NULL,
      opinometro_id = NULL,
      bd_respuestas = NULL,
      snapshot_original = NULL,
      cuestionario = NULL,
      muestra_diseno = NULL,
      auditoria_telefonica = NULL,
      bd_eliminadas_regla = NULL,
      bd_categorias = NULL,
      shp = NULL,
      shp_completo = NULL,
      tipo_encuesta = NULL,
      patron = NULL,
      mantener = NULL,
      Respuestas_proc = NULL,
      # CAMPOS INTERMEDIOS (Resultados de métodos)
      bd_respuestas_preparadas = NULL,
      nuevos_registros_snapshot = NULL,
      nuevos_registros_cluster = NULL,
      # CAMPO FINAL (Resultado principal)
      diseño_muestral = NULL,
      sbj_eliminadas_auditoria = NULL,
      sbj_eliminadas_regla = NULL,
      marcas_eliminacion = NULL,
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
      #' @description Asigna todos los insumos a la clase sin procesarlos.
      initialize = function(
        pool = NULL,
        opinometro_id = NULL,
        bd_respuestas = NULL,
        bd_snapshot = NULL,
        bd_categorias = NULL,
        cuestionario = NULL,
        muestra = NULL,
        mantener = "",
        auditoria_telefonica = NULL,
        bd_eliminadas_regla = NULL,
        bd_eliminadas_reglas = NULL,
        shp = NULL,
        tipo_encuesta = NULL,
        patron = NULL
      ) {
        self$pool <- pool
        self$opinometro_id <- opinometro_id
        self$bd_respuestas <- bd_respuestas
        self$snapshot_original <- bd_snapshot
        self$cuestionario <- Cuestionario$new(documento = cuestionario, patron)
        self$muestra_diseno <- muestra
        self$auditoria_telefonica <- auditoria_telefonica
        self$bd_eliminadas_regla <- bd_eliminadas_regla
        self$shp <- shp
        self$tipo_encuesta <- tipo_encuesta
        self$patron <- patron
        self$bd_categorias <- bd_categorias

        # Backward compat: accept the old plural parameter name
        if (!is.null(bd_eliminadas_reglas) && is.null(self$bd_eliminadas_regla)) {
          warning("El parámetro 'bd_eliminadas_reglas' está obsoleto; usa 'bd_eliminadas_regla' (singular).")
          self$bd_eliminadas_regla <- bd_eliminadas_reglas
        }

        if (!is.null(muestra) && !is.null(shp)) {
          un <- self$muestra_diseno$niveles %>%
            filter(nivel == self$muestra_diseno$ultimo_nivel)
          nivel <- un |>
            unite(nivel, tipo, nivel) |>
            pull(nivel)
          var_n <- un |> pull(variable)

          self$shp_completo <- shp

          self$shp <-
            shp$shp %>%
            purrr::pluck(var_n) %>%
            inner_join(
              muestra$muestra %>%
                purrr::pluck(var_n) %>%
                unnest(data) %>%
                distinct(
                  !!rlang::sym(var_n) := !!rlang::sym(var_n),
                  !!rlang::sym(nivel)
                )
            )
        }
        self$mantener <- mantener

        message("Objeto Preproceso inicializado.")
        invisible(self)
      },
      #' @description Prepara la base de respuestas cruda (GPS y variables clave).
      preparar_respuestas = function() {
        if (is.null(self$bd_respuestas)) {
          stop(
            "La base de respuestas cruda (bd_respuestas) no ha sido proporcionada."
          )
        }
        message("Preparando respuestas crudas...")

        bd_geo <- self$bd_respuestas %>%
          {
            if (sum(grepl("gps_", names(.))) > 0) {
              filter(., !is.na(INT1)) |>
                select(Id, contains("gps_")) |>
                tidyr::pivot_longer(cols = -Id, values_to = "gps") |>
                filter(!is.na(gps), gps != "") |>
                group_by(Id) |>
                mutate(INT = row_number()) |>
                ungroup() |>
                filter(INT == max(INT), .by = Id) |>
                select(Id, gps)
            } else if ("gps" %in% names(.)) {
              select(., Id, gps)
            } else {
              tibble(Id = integer(0), gps = character(0))
            }
          }

        # Detalle por intento (INT_1..INT_15): cada renglón acumula el
        # resultado de cada toque de puerta. El `-matches("^INT_?[0-9]+$")` de
        # abajo los descarta (junto con gps/aux/etc.), así que se re-adjuntan
        # desde el crudo para que lleguen al snapshot. Son el insumo de
        # `pivotar_intentos()` / `derivar_registro_contactos()` (registro de
        # contactos y tasa de respuesta por sección para la sobremuestra), que
        # esperan leerlos DEL snapshot.
        #
        # IMPORTANTE: el drop de las numeradas DEBE ser un `matches()` anclado,
        # NO `contains("INT")`. `tidyselect::contains()` hace coincidencia por
        # subcadena e `ignore.case = TRUE` por defecto, así que `"INT"` también
        # tiraba variables de cuestionario que contienen las letras "int"
        # (`internet`, `conoce_interurbano`, `op_interurbano`); al no re-
        # adjuntarse quedaban 100% NULL en el snapshot.
        cols_intento <- self$bd_respuestas |>
          select(Id, matches("^INT_?[0-9]+$"))

        self$bd_respuestas_preparadas <- self$bd_respuestas |>
          select(
            -contains(c("gps", "intentos_", "introduccion", "aux_")),
            -matches("^INT_?[0-9]+$")
          ) |>
          left_join(bd_geo, by = "Id") |>
          left_join(cols_intento, by = "Id") |>
          mutate(
            # `missing = "Otro"` es imprescindible: los registros NO efectivos
            # (rechazos, "No aplica", etc.) traen `finalizar = NA`, y sin este
            # argumento `if_else(NA, ...)` devuelve NA. Ese NA hacía que luego
            # `retirar_no_efectivas()` —que filtra `TipoRegistro != "Efectivo"`—
            # los descartara (NA != "Efectivo" es NA => se caen), por lo que
            # jamás llegaban al snapshot. Con "Otro" se conservan como no
            # efectivas y `bind_rows(self$no_efectivas)` los reincorpora al
            # snapshot; el diseño los sigue excluyendo vía `filtrar_efectivas()`.
            TipoRegistro = if_else(
              finalizar == "Finalizar",
              "Efectivo",
              "Otro",
              missing = "Otro"
            ),
            cluster = as.numeric(as.character(cluster))
          )

        message("Respuestas preparadas exitosamente.")
        invisible(self)
      },
      # AÑADIR DENTRO DE `public = list(...)`

      #' @description Procesa las nuevas respuestas: filtra, limpia y las prepara
      #' para ser añadidas al snapshot.
      procesar_nuevas_entradas = function() {
        # --- VALIDACIÓN INICIAL ---
        if (is.null(self$bd_respuestas_preparadas)) {
          stop("Primero debes ejecutar $preparar_respuestas().")
        }

        # --- Guard: snapshot_original debe ser un tibble en memoria ---
        # Un tbl lazy hace que nrow() devuelva NA, lo que silenciosamente
        # deshabilita el anti_join y trata todos los registros como nuevos,
        # causando duplicados en el snapshot.
        if (!is.null(self$snapshot_original) &&
            inherits(self$snapshot_original, "tbl_lazy")) {
          message(
            "snapshot_original es una referencia lazy a la BD; ",
            "ejecutando collect() antes del anti_join..."
          )
          self$snapshot_original <- dplyr::collect(self$snapshot_original)
        }

        # --- Reset de vectores (evita arrastre entre corridas) ---
        self$sbj_eliminadas_auditoria <- numeric()
        self$sbj_eliminadas_regla <- numeric()

        # ============================================================
        # 1) Definir universos: TODO vs NUEVO
        # ============================================================
        respuestas_todas <- self$bd_respuestas_preparadas

        respuestas_nuevas <- respuestas_todas
        if (
          !is.null(self$snapshot_original) && nrow(self$snapshot_original) > 0
        ) {
          respuestas_nuevas <- respuestas_nuevas |>
            dplyr::anti_join(self$snapshot_original, by = c("Id" = "SbjNum"))
        }

        # ============================================================
        # 2) Detectar eliminaciones GLOBALMENTE (sin anti_join)
        #    Esto permite afectar registros ya existentes en snapshot.
        # ============================================================
        marcadas_todas <- private$marcar_eliminadas_auditoria(respuestas_todas)
        marcadas_todas <- private$marcar_eliminadas_por_regla(marcadas_todas)

        self$sbj_eliminadas_auditoria <- marcadas_todas |>
          dplyr::filter(eliminada_auditoria == 1) |>
          dplyr::pull(Id) |>
          unique()

        self$sbj_eliminadas_regla <- marcadas_todas |>
          dplyr::filter(eliminada_regla == 1) |>
          dplyr::pull(Id) |>
          unique()

        # Estado completo (0 y 1) de cada registro evaluado en esta corrida.
        # actualizar_bd() lo usa para sincronizar el snapshot en ambas
        # direcciones: al borrar una regla o corregir una auditoría, los
        # registros afectados se restauran en la siguiente actualización.
        self$marcas_eliminacion <- marcadas_todas |>
          dplyr::group_by(Id) |>
          dplyr::summarize(
            eliminada_auditoria = as.integer(max(eliminada_auditoria, na.rm = TRUE)),
            eliminada_regla     = as.integer(max(eliminada_regla,     na.rm = TRUE)),
            .groups = "drop"
          ) |>
          dplyr::rename(SbjNum = Id)

        # ============================================================
        # 3) Si no hay nuevos registros, no procesar pesado,
        #    pero sí permitir que actualizar_bd() haga UPDATE de eliminadas
        # ============================================================
        if (nrow(respuestas_nuevas) == 0) {
          message(
            "No hay nuevos registros que procesar (pero sí se sincronizarán eliminaciones)."
          )
          self$nuevos_registros_snapshot <- NULL
          self$nuevos_registros_cluster <- NULL
          self$Respuestas_proc <- NULL
          return(invisible(self))
        }

        # ============================================================
        # 4) Procesar SOLO NUEVOS (flujo original preservado)
        # ============================================================
        # Los IDs eliminados ya fueron calculados globalmente en el paso 2.
        # Reutilizamos esos vectores con un simple %in% en lugar de volver a
        # ejecutar la detección completa (O(n×r) bucles para reglas de fecha).
        respuestas_con_marcas <- respuestas_nuevas |>
          dplyr::mutate(
            eliminada_auditoria = dplyr::if_else(
              Id %in% self$sbj_eliminadas_auditoria, 1L, 0L
            ),
            eliminada_regla = dplyr::if_else(
              Id %in% self$sbj_eliminadas_regla, 1L, 0L
            )
          )

        message(glue::glue(
          "Se procesarán {nrow(respuestas_con_marcas)} nuevos registros."
        ))
        # 4.1) Instanciar Opinómetro y traer cuestionario
        opinometro <- Opinometro_proc$new(
          bd_respuestas = respuestas_con_marcas,
          id_cuestionarioOpinometro = self$opinometro_id,
          pool = self$pool,
          diccionario = self$cuestionario$diccionario
        )

        # 4.2) Variables de nivel muestral
        un <- self$muestra_diseno$niveles %>%
          dplyr::filter(nivel == self$muestra_diseno$ultimo_nivel)

        nivel <- un |>
          tidyr::unite(nivel, tipo, nivel) |>
          dplyr::pull(nivel)

        var_n <- un |> dplyr::pull(variable)

        # 4.3) Aplicar transformaciones complejas
        catalogo_para_respuestas <- catalogo_variables |>
          dplyr::bind_rows(
            self$cuestionario$diccionario |>
              dplyr::select(variable = llaves) |>
              dplyr::mutate(
                plataforma   = "cuestionario",
                primer_nivel = "cuestionario",
                segundo_nivel = dplyr::if_else(
                  variable %in% c("cluster", "edad", "sexo"),
                  "sistema",
                  "cuestionario"
                )
              )
          )

        self$Respuestas_proc <- Respuestas_proc$new(
          base = opinometro$bd_respuestas_cuestionario |>
            dplyr::mutate(cluster_0 = SbjNum),
          Preproceso = self,
          catalogo = catalogo_para_respuestas,
          muestra_completa = self$muestra_diseno,
          nivel = nivel,
          var_n = var_n
        )

        # ============================================================
        # 5) ASIGNACIÓN FINAL (append + correcciones)
        # ============================================================
        if ("distancia" %in% colnames(self$Respuestas_proc$base)) {
          self$nuevos_registros_snapshot <- self$Respuestas_proc$base |>
            dplyr::rename(INT = intento_efectivo) |>
            dplyr::select(-dplyr::contains("Pregunta")) |>
            dplyr::mutate(distancia = as.character(distancia))
        } else {
          self$nuevos_registros_snapshot <- self$Respuestas_proc$base |>
            dplyr::rename(INT = intento_efectivo) |>
            dplyr::select(-dplyr::contains("Pregunta")) |>
            dplyr::mutate(distancia = 0)
        }

        self$nuevos_registros_cluster <- self$Respuestas_proc$cluster_corregido

        invisible(self)
      },
      actualizar_bd = function() {
        message("Iniciando la persistencia de cambios en la base de datos...")

        # Validar opinometro_id antes de construir nombres de tabla con él.
        # Un ID no-entero generaría un nombre de tabla inválido que corrompería
        # todas las queries de esta sesión.
        private$validar_opinometro_id()

        # --- Bloque 1 (atómico): insertar nuevos registros + corregir clusters ---
        # Ambas operaciones sólo afectan registros nuevos. Si cualquiera falla
        # se hace rollback de ambas; el snapshot queda idéntico al estado previo.
        error_registros <- NULL
        con <- pool::poolCheckout(self$pool)
        on.exit(pool::poolReturn(con), add = TRUE)

        tryCatch({
          DBI::dbBegin(con)
          private$agregar_nuevos_registros(con)
          private$actualizar_clusters_corregidos(con)
          DBI::dbCommit(con)
        }, error = function(e) {
          DBI::dbRollback(con)
          error_registros <<- e
          message(glue::glue("- Rollback: {e$message}"))
        })

        # --- Bloque 2 (idempotente): marcado de eliminaciones en todo el snapshot ---
        # Corre siempre, independientemente del resultado del bloque 1, porque
        # las reglas de eliminación se aplican sobre registros existentes y no
        # deben quedar bloqueadas por un fallo en la inserción de nuevos.
        # Se pasa `con` para reusar la conexión del bloque 1 y evitar un segundo
        # poolCheckout simultáneo que puede causar deadlock si maxSize = 1.
        private$marcar_registros_eliminados(con)

        if (!is.null(error_registros)) {
          stop(glue::glue(
            "Eliminaciones actualizadas correctamente, pero los nuevos registros ",
            "no pudieron insertarse: {error_registros$message}"
          ))
        }

        message("La base de datos ha sido actualizada exitosamente.")
        return(invisible(self))
      },
      generar_diseno = function() {
        message("Generando diseño muestral a partir del snapshot final...")
        snapshot_id <- glue::glue("snapshot_id_{self$opinometro_id}")

        # 1. Leer y filtrar el snapshot
        snapshot_final <- dplyr::tbl(self$pool, snapshot_id) |> dplyr::collect()
        snapshot_valido <- private$filtrar_efectivas(snapshot_final)

        message(glue::glue(
          "Se usarán {nrow(snapshot_valido)} entrevistas válidas para el diseño."
        ))

        # 2. Obtener información del plan muestral
        diseno_plan <- self$muestra_diseno
        un <- diseno_plan$niveles |> filter(nivel == diseno_plan$ultimo_nivel)
        nivel <- un |> tidyr::unite(nivel, tipo, nivel) |> pull(nivel)
        var_n <- un |> pull(variable)

        # 3. Instanciar la clase Muestra (necesaria para la preparación)
        muestra_obj <- Muestra$new(
          muestra = diseno_plan,
          respuestas = snapshot_valido,
          nivel = nivel,
          var_n = var_n
        )

        # 4. PREPARAR LOS DATOS PARA PONDERAR usando el método privado
        snap_a_ponderar <- private$preparar_datos_ponderacion(
          snapshot_valido,
          muestra_obj,
          var_n
        )

        # 5. Extraer el diseño final con los datos ya preparados
        muestra_obj$extraer_diseno(
          respuestas = snap_a_ponderar,
          marco_muestral = muestra_obj$muestra$poblacion$marco_muestral,
          tipo_encuesta = self$tipo_encuesta,
          sin_peso = F,
          rake = T
        )

        # 6. Asignar el resultado final
        self$diseño_muestral <- muestra_obj

        message("¡Diseño muestral ponderado generado exitosamente!")
        invisible(self)
      },
      #' @description Prepara las variables DR-MNAR (instrumento `drmnar_z`,
      #'  respuesta por protocolo `drmnar_r` y dicotómicas por pregunta)
      #'  sobre los registros nuevos del snapshot. Llamar DESPUÉS de
      #'  `procesar_nuevas_entradas()` y ANTES de `actualizar_bd()` para que
      #'  las variables queden persistidas en el snapshot (flujo AppAuditoria).
      #' @param instrumento_campo Pregunta aleatoria del filtro temático en
      #'  campo (NA = brazo control), p. ej. "gustos_aleatorio_50".
      #' @param opcion_politica Valor del filtro que continúa el módulo
      #'  político.
      #' @param preguntas Lista nombrada `list(pregunta = categoria(s))` de
      #'  dicotómicas a reservar para los cálculos por pregunta.
      preparar_variables_dicotomicas_drmnar = function(instrumento_campo,
                                                       opcion_politica = "Política",
                                                       preguntas = NULL) {
        if (is.null(self$nuevos_registros_snapshot)) {
          stop(
            "No hay registros nuevos procesados: corre ",
            "procesar_nuevas_entradas() antes de preparar las variables ",
            "DR-MNAR."
          )
        }
        self$nuevos_registros_snapshot <- preparar_variables_drmnar(
          bd = self$nuevos_registros_snapshot,
          instrumento_campo = instrumento_campo,
          opcion_politica = opcion_politica,
          preguntas = preguntas
        )
        message("Variables DR-MNAR preparadas en los registros nuevos.")
        invisible(self)
      },
      #' @description Genera el segundo tipo de diseño (bundle `diseno_drmnar`)
      #'  a partir del diseño muestral ponderado del flujo normal: corre el
      #'  diagnóstico de no respuesta no ignorable por pregunta y aplica la
      #'  regla de decisión DR-MNAR vs Raking (con override manual opcional).
      #'  Requiere haber corrido `generar_diseno()` (o lo ejecuta).
      #' @param preguntas Lista nombrada `list(pregunta = categoria(s))`.
      #' @param covariables Covariables de los modelos (individuales y/o
      #'  seccionales ya unidas a las variables del diseño).
      #' @param instrumento_campo Pregunta aleatoria del filtro en campo.
      #' @param opcion_politica Valor del filtro que continúa el módulo.
      #' @param covariables_seccion Tibble opcional de
      #'  `construir_covariables_seccion()` a unir por sección.
      #' @param entidad Clave "EE" para normalizar la llave de sección.
      #' @param subconjuntos Lista nombrada de vectores lógicos.
      #' @param override Tibble `pregunta`, `decision` para forzar decisiones.
      #' @param ... Argumentos adicionales para `diagnosticar_norespuesta()`.
      generar_diseno_drmnar = function(preguntas,
                                       covariables = NULL,
                                       instrumento_campo = "gustos_aleatorio_50",
                                       opcion_politica = "Política",
                                       covariables_seccion = NULL,
                                       entidad = NULL,
                                       subconjuntos = NULL,
                                       override = NULL,
                                       ...) {
        if (is.null(self$diseño_muestral)) {
          self$generar_diseno()
        }
        diseno <- self$diseño_muestral$diseno
        vars <- diseno$variables
        if (!"drmnar_z" %in% names(vars)) {
          vars <- preparar_variables_drmnar(
            bd = vars,
            instrumento_campo = instrumento_campo,
            opcion_politica = opcion_politica,
            preguntas = preguntas
          )
        }
        if (!is.null(covariables_seccion)) {
          vars <- unir_covariables_individuo(
            respuestas = vars,
            covariables_seccion = covariables_seccion,
            entidad = entidad
          )
        }
        diseno$variables <- vars

        # función exportada del paquete (no este método)
        generar_diseno_drmnar(
          diseno = diseno,
          preguntas = preguntas,
          covariables = covariables,
          subconjuntos = subconjuntos,
          override = override,
          ...
        )
      }
    ),
    private = list(
      marcar_eliminadas_auditoria = function(base) {
        auditoria <- self$auditoria_telefonica
        if (is.null(auditoria) || nrow(auditoria) == 0) {
          return(base |> mutate(eliminada_auditoria = 0))
        }
        if (!("Id" %in% names(base)) || !("SbjNum" %in% names(auditoria))) {
          warning("Faltan 'Id' o 'SbjNum'. No se pudo marcar por auditoría.")
          return(base |> mutate(eliminada_auditoria = 0))
        }
        if (is.character(auditoria$SbjNum)) {
          auditoria <- auditoria |> mutate(SbjNum = readr::parse_double(SbjNum))
        }
        ids_eliminados <- auditoria |>
          distinct(SbjNum) |>
          pull()
        base <- base |>
          mutate(eliminada_auditoria = if_else(Id %in% ids_eliminados, 1, 0))
        return(base)
      },

      marcar_eliminadas_por_regla = function(base) {
        if (
          is.null(self$bd_eliminadas_regla) ||
            nrow(self$bd_eliminadas_regla) == 0
        ) {
          return(base |> mutate(eliminada_regla = 0))
        }

        ids_a_eliminar <- private$obtener_ids_eliminadas_por_regla(base)

        base <- base |>
          mutate(eliminada_regla = if_else(Id %in% ids_a_eliminar, 1, 0))

        return(base)
      },

      obtener_ids_eliminadas_por_regla = function(base) {
        reglas <- self$bd_eliminadas_regla

        # Reglas por fecha
        reglas_fecha <- reglas |>
          filter(!is.na(fecha_inicio) & is.na(UsuarioNum))
        vec_elim_fech <- c()
        if (nrow(reglas_fecha) > 0) {
          for (i in 1:nrow(reglas_fecha)) {
            ids <- base |>
              filter(
                FechaInicio >= reglas_fecha$fecha_inicio[i] &
                  FechaInicio <= reglas_fecha$fecha_fin[i]
              ) |>
              pull(Id)
            vec_elim_fech <- c(vec_elim_fech, ids)
          }
        }

        # Reglas por usuario
        reglas_usr <- reglas |> filter(is.na(fecha_inicio) & !is.na(UsuarioNum))
        vec_elim_usr <- c()
        if (nrow(reglas_usr) > 0) {
          vec_elim_usr <- base |>
            filter(
              as.character(UsuarioNum) %in% as.character(reglas_usr$UsuarioNum)
            ) |>
            pull(Id)
        }

        # Reglas por fecha y usuario
        reglas_fecha_usr <- reglas |>
          filter(!is.na(fecha_inicio) & !is.na(UsuarioNum))
        vec_elim_usr_fech <- c()
        if (nrow(reglas_fecha_usr) > 0) {
          for (i in 1:nrow(reglas_fecha_usr)) {
            ids <- base |>
              filter(
                as.character(UsuarioNum) ==
                  as.character(reglas_fecha_usr$UsuarioNum[i])
              ) |>
              filter(
                FechaInicio >= reglas_fecha_usr$fecha_inicio[i] &
                  FechaInicio <= reglas_fecha_usr$fecha_fin[i]
              ) |>
              pull(Id)
            vec_elim_usr_fech <- c(vec_elim_usr_fech, ids)
          }
        }

        # Combinar y devolver IDs únicos
        return(unique(c(vec_elim_fech, vec_elim_usr, vec_elim_usr_fech)))
      },
      #' @description Filtra un dataframe para mantener solo las entrevistas válidas.
      #' @param base El dataframe a filtrar (ej. el snapshot).
      #' @return Un dataframe filtrado.
      filtrar_efectivas = function(base) {
        base |>
          filter(
            TipoRegistro == "Efectivo",
            eliminada_proceso == 0 | is.na(eliminada_proceso),
            eliminada_auditoria == 0 | is.na(eliminada_auditoria),
            eliminada_regla == 0 | is.na(eliminada_regla)
          )
      },
      #' @description Prepara el snapshot para la ponderación, añadiendo variables
      #' de diseño, demográficos estandarizados y la región.
      #' @param snap El dataframe del snapshot válido.
      #' @param muestra_obj La instancia de la clase Muestra.
      #' @return Un dataframe listo para ser ponderado.
      preparar_datos_ponderacion = function(snap, muestra_obj, var_n) {
        vars_join <- c(
          var_n,
          names(muestra_obj$base)[is.na(match(
            names(muestra_obj$base),
            names(snap)
          ))]
        )

        snap <- snap %>%
          inner_join(muestra_obj$base %>% select(all_of(vars_join)))

        # --- 2. Creación de variables demográficas según tipo de encuesta ---
        if (isTRUE(self$tipo_encuesta == "inegi")) {
          snap <- snap %>%
            mutate(
              rango_edad = as.character(cut(
                as.integer(edad),
                c(17, 24, 59, 200),
                c("18A24", "25A59", "60YMAS")
              )),
              sexo = if_else(sexo == "Mujer", "F", "M")
            )
        }

        if (isTRUE(self$tipo_encuesta == "ine")) {
          snap <- snap %>%
            mutate(
              rango_edad = cut(
                as.numeric(edad),
                c(17, 24, 39, 59, Inf),
                labels = c("18A24", "25A39", "40A59", "60YMAS")
              ),
              sexo = if_else(sexo == "Mujer", "F", "M")
            )
        }

        # --- 3. Join de la variable de región si existe en el diseño ---
        if (sum(grepl("region", muestra_obj$muestra$niveles$variable)) > 0) {
          var_reg <- muestra_obj$muestra$niveles %>%
            filter(variable == "region") %>%
            unite("var_reg", c(tipo, nivel)) %>%
            pull(var_reg)
          snap <- snap %>%
            inner_join(
              muestra_obj$muestra$poblacion$marco_muestral %>%
                distinct(across(all_of(var_reg)), region),
              by = var_reg
            )
        }

        return(snap)
      },
      #' @description Agrega los nuevos registros procesados a la tabla snapshot.
      #' @details
      #' Toma el tibble de la clase hija Respuestas_proc, le añade una columna
      #' de auditoría con la fecha y hora de la actualización, y lo anexa a la
      #' tabla snapshot correspondiente.
      agregar_nuevos_registros = function(con) {
        nuevos_registros <- self$nuevos_registros_snapshot

        if (is.null(nuevos_registros) || nrow(nuevos_registros) == 0) {
          message("- No hay nuevos registros para agregar.")
          return(invisible(self))
        }

        nombre_snapshot <- glue::glue("snapshot_id_{self$opinometro_id}")

        # Defensa final contra duplicados: el anti_join de
        # procesar_nuevas_entradas() se hizo contra una lectura del snapshot
        # que pudo quedar desactualizada si otra instancia de la aplicación
        # insertó registros mientras esta corrida procesaba. Se re-verifica
        # dentro de la transacción, justo antes de insertar.
        ids_existentes <- DBI::dbGetQuery(
          con,
          glue::glue("SELECT SbjNum FROM {nombre_snapshot}")
        )$SbjNum

        duplicados <- nuevos_registros$SbjNum %in% ids_existentes
        if (any(duplicados)) {
          message(glue::glue(
            "- Se omiten {sum(duplicados)} registros que ya existen en el snapshot ",
            "(insertados por otra corrida concurrente)."
          ))
          nuevos_registros <- nuevos_registros[!duplicados, ]
        }

        if (nrow(nuevos_registros) == 0) {
          message("- No quedaron registros nuevos por agregar.")
          return(invisible(self))
        }

        hora_mexico <- lubridate::with_tz(Sys.time(), "America/Mexico_City")
        registros_para_subir <- nuevos_registros %>%
          dplyr::mutate(corte_actualizacion = hora_mexico)

        DBI::dbAppendTable(con, nombre_snapshot, registros_para_subir)

        message(glue::glue(
          "- Se agregaron {nrow(registros_para_subir)} nuevos registros a '{nombre_snapshot}'."
        ))

        return(invisible(self))
      },
      #' @description Actualiza el snapshot marcando entrevistas eliminadas.
      #' @details
      #' Ejecuta sentencias UPDATE para establecer el flag de eliminación
      #' (ej. eliminada_auditoria = 1) basándose en los SbjNum identificados
      #' en el proceso de limpieza.
      marcar_registros_eliminados = function(con) {
        marcas <- self$marcas_eliminacion

        if (is.null(marcas) || nrow(marcas) == 0) {
          message("- No hay registros evaluados para sincronizar eliminaciones.")
          return(invisible(self))
        }

        # Validar que los IDs sean numéricos y finitos antes de usarlos en el join.
        private$validar_ids_sql(marcas$SbjNum, "marcas_eliminacion$SbjNum")

        nombre_snapshot <- glue::glue("snapshot_id_{self$opinometro_id}")
        nombre_temp     <- "#marcas_eliminacion_temp"

        DBI::dbWriteTable(con, name = nombre_temp, value = marcas,
                          temporary = TRUE, overwrite = TRUE)

        # Sincronización en ambas direcciones (0 -> 1 y 1 -> 0), acotada a los
        # SbjNum evaluados en esta corrida. Marcar sólo con SET = 1 dejaba las
        # eliminaciones como irreversibles: borrar una regla equivocada no
        # restauraba las entrevistas ya marcadas.
        sql_sync <- glue::glue(
          "
UPDATE target
SET
    eliminada_auditoria = mods.eliminada_auditoria,
    eliminada_regla     = mods.eliminada_regla
FROM {nombre_snapshot} AS target
INNER JOIN {nombre_temp} AS mods
    ON target.SbjNum = mods.SbjNum
WHERE
    ISNULL(target.eliminada_auditoria, -1) <> mods.eliminada_auditoria
    OR ISNULL(target.eliminada_regla, -1) <> mods.eliminada_regla;
"
        )

        filas <- DBI::dbExecute(con, sql_sync)
        message(glue::glue(
          "- Flags de eliminación sincronizados: {filas} filas actualizadas ",
          "({length(self$sbj_eliminadas_auditoria)} por auditoría, ",
          "{length(self$sbj_eliminadas_regla)} por regla)."
        ))

        # Los registros que entraron al snapshot por la vía de eliminadas no
        # pasan por el cálculo de eliminada_proceso y quedan en NULL; en SQL,
        # filtros como `eliminada_proceso != 1` descartan NULL silenciosamente.
        filas_proceso <- DBI::dbExecute(con, glue::glue(
          "UPDATE {nombre_snapshot}
           SET eliminada_proceso = 0
           WHERE eliminada_proceso IS NULL"
        ))
        if (filas_proceso > 0) {
          message(glue::glue(
            "- Se normalizó eliminada_proceso NULL -> 0 en {filas_proceso} filas."
          ))
        }

        return(invisible(self))
      },
      #' @description Aplica las correcciones de clúster al snapshot.
      #' @details
      #' Toma los datos del campo `self$nuevos_registros_cluster`, los sube
      #' a una tabla temporal y ejecuta un UPDATE masivo en la tabla snapshot
      #' para reflejar los clústeres corregidos.
      #' @description Valida que `opinometro_id` sea un entero positivo finito.
      #' @details Se llama al inicio de cualquier método que construya un nombre
      #'   de tabla con ese valor. Lanza un error descriptivo si la validación falla.
      validar_opinometro_id = function() {
        id <- self$opinometro_id
        if (
          is.null(id) ||
          length(id) != 1 ||
          !is.numeric(id) ||
          !is.finite(id) ||
          id != trunc(id) ||
          id <= 0
        ) {
          stop(glue::glue(
            "opinometro_id debe ser un entero positivo finito; ",
            "valor recibido: {deparse(id)}"
          ))
        }
        invisible(TRUE)
      },
      #' @description Valida que un vector de IDs sea numérico antes de interpolarlo en SQL.
      #' @param ids Vector a validar.
      #' @param nombre Nombre del campo (para el mensaje de error).
      validar_ids_sql = function(ids, nombre) {
        if (length(ids) == 0) return(invisible(TRUE))
        if (!is.numeric(ids)) {
          stop(glue::glue(
            "'{nombre}' debe ser un vector numérico para poder interpolarlo ",
            "en SQL; clase recibida: {class(ids)[1]}"
          ))
        }
        if (any(!is.finite(ids))) {
          stop(glue::glue(
            "'{nombre}' contiene valores NA o infinitos que no pueden ",
            "interpolarse en SQL: {paste(ids[!is.finite(ids)], collapse = ', ')}"
          ))
        }
        invisible(TRUE)
      },
      actualizar_clusters_corregidos = function(con) {
        correcciones <- self$nuevos_registros_cluster

        if (is.null(correcciones) || nrow(correcciones) == 0) {
          message("- No hay correcciones de clúster para aplicar.")
          return(invisible(self))
        }

        nombre_snapshot <- glue::glue("snapshot_id_{self$opinometro_id}")
        nombre_temp     <- "#cluster_corregido_temp"

        sql_add_col <- glue::glue(
          "
IF COL_LENGTH('{nombre_snapshot}', 'cluster_original') IS NULL
BEGIN
  ALTER TABLE {nombre_snapshot}
  ADD cluster_original NVARCHAR(255) NULL;
END
"
        )

        DBI::dbExecute(con, sql_add_col)

        DBI::dbWriteTable(con, name = nombre_temp, value = correcciones,
                          temporary = TRUE, overwrite = TRUE)

        sql_update <- glue::glue(
          "
UPDATE target
SET
    cluster_corregida = target.cluster,
    cluster = mods.nueva,
    corregida = 1
FROM {nombre_snapshot} AS target
INNER JOIN {nombre_temp} AS mods
    ON target.SbjNum = mods.SbjNum
WHERE
    ISNULL(CONVERT(NVARCHAR(255), target.cluster), '') <> ISNULL(CONVERT(NVARCHAR(255), mods.nueva), '');
"
        )

        filas <- DBI::dbExecute(con, sql_update)
        message(glue::glue("- Clústeres corregidos: {filas} filas afectadas."))

        return(invisible(self))
      }
    )
  )
