# Preparación de variables DR-MNAR al final del preprocesamiento --------
#
# Estas funciones formalizan el contrato de datos del protocolo de no
# respuesta (Secciones 1-2 del Anexo Técnico) sobre el snapshot ya
# preprocesado:
#
#   - drmnar_z: brazo del instrumento aleatorizado (0 = control directo,
#     1 = tratamiento con filtro temático). En campo, el filtro NO
#     administrado se registra como NA en la pregunta aleatoria (p. ej.
#     `gustos_aleatorio_50` en Chihuahua Ene-2026).
#   - drmnar_tema: tema elegido en el filtro (NA en el brazo control).
#   - drmnar_r: respuesta del módulo político BAJO PROTOCOLO. Los
#     desertores del filtro (tratamiento que no eligió "Política") son no
#     respuesta aunque el campo haya continuado el cuestionario, como
#     ocurrió en Chihuahua: sus respuestas observadas se conservan en las
#     columnas originales (útiles para validación) pero se enmascaran en
#     las variables de cálculo.
#   - drmnar_y_<pregunta>: dicotómicas por pregunta reservadas para los
#     cálculos DR-MNAR (NA cuando drmnar_r = 0 o falta el ítem).

#' Prepara las variables DR-MNAR sobre el snapshot preprocesado
#'
#' Crea el instrumento `drmnar_z`, el tema del filtro `drmnar_tema`, el
#' indicador de respuesta por protocolo `drmnar_r` y, para cada pregunta
#' solicitada, la variable dicotómica `drmnar_y_<pregunta>` que se reserva
#' para los cálculos por pregunta (diagnóstico de no ignorabilidad y
#' estimación DR-MNAR).
#'
#' @param bd Snapshot preprocesado (una fila por contacto/entrevista).
#' @param instrumento_campo Nombre de la pregunta aleatoria del filtro
#'   temático en campo (NA = filtro no administrado = brazo control).
#' @param opcion_politica Valor del filtro que continúa el módulo político.
#' @param preguntas Lista nombrada `list(pregunta = categoria(s))` con las
#'   preguntas a dicotomizar (la categoría define y = 1).
#' @return `bd` con las columnas `drmnar_z`, `drmnar_tema`, `drmnar_r` y
#'   una `drmnar_y_<pregunta>` por cada pregunta solicitada.
#' @examples
#' \dontrun{
#' snapshot <- preparar_variables_drmnar(
#'   snapshot,
#'   instrumento_campo = "gustos_aleatorio_50",
#'   preguntas = list(
#'     "conoce_cruz" = "Sí lo conoce",
#'     "conoce_andrea" = "Sí lo conoce"
#'   )
#' )
#' }
#' @export
preparar_variables_drmnar <- function(bd,
                                      instrumento_campo,
                                      opcion_politica = "Política",
                                      preguntas = NULL) {
  if (!instrumento_campo %in% names(bd)) {
    stop(
      "No existe la columna `", instrumento_campo, "` en el snapshot: ",
      "se requiere la pregunta aleatoria del filtro temático (protocolo ",
      "de la Sección 2 del Anexo Técnico)."
    )
  }

  filtro <- bd[[instrumento_campo]]
  bd$drmnar_z <- as.numeric(!is.na(filtro))
  bd$drmnar_tema <- ifelse(is.na(filtro), NA_character_, as.character(filtro))
  # respuesta del módulo político bajo protocolo: el control pasa directo;
  # el tratamiento solo si eligió la opción política
  bd$drmnar_r <- as.numeric(is.na(filtro) | filtro == opcion_politica)

  if (!is.null(preguntas)) {
    if (is.null(names(preguntas)) || any(names(preguntas) == "")) {
      stop("`preguntas` debe ser una lista nombrada pregunta = categoria(s).")
    }
    for (preg in names(preguntas)) {
      if (!preg %in% names(bd)) {
        stop("No existe la pregunta `", preg, "` en el snapshot.")
      }
      categoria <- preguntas[[preg]]
      y <- as.numeric(bd[[preg]] %in% categoria)
      y[is.na(bd[[preg]])] <- NA_real_
      # bajo protocolo, la respuesta de los desertores no se observa
      y[bd$drmnar_r == 0] <- NA_real_
      bd[[paste0("drmnar_y_", preg)]] <- y
    }
  }

  bd
}

#' Flujo de tamaños de muestra por brazo del instrumento (Figure 2)
#'
#' Reproduce los conteos del diagrama de flujo de la Figura 2 de Bailey:
#' contactos por brazo, respuesta al módulo político y desglose de la
#' deserción del filtro temático por tema elegido.
#'
#' @param bd Snapshot con las variables de [preparar_variables_drmnar()].
#' @return Tibble con columnas `brazo`, `etapa`, `detalle`, `n`.
#' @export
resumen_flujo_norespuesta <- function(bd) {
  if (!all(c("drmnar_z", "drmnar_r") %in% names(bd))) {
    stop("Corre primero preparar_variables_drmnar() sobre el snapshot.")
  }
  control <- bd[bd$drmnar_z == 0, ]
  trat <- bd[bd$drmnar_z == 1, ]

  desercion <- trat[trat$drmnar_r == 0, ]
  desglose <- desercion |>
    dplyr::count(drmnar_tema, name = "n") |>
    dplyr::transmute(
      brazo = "Tratamiento",
      etapa = "Desertó del filtro",
      detalle = drmnar_tema,
      n = n
    )

  dplyr::bind_rows(
    tibble::tibble(
      brazo = c("Total", "Control", "Tratamiento"),
      etapa = "Contactos",
      detalle = NA_character_,
      n = c(nrow(bd), nrow(control), nrow(trat))
    ),
    tibble::tibble(
      brazo = c("Control", "Tratamiento"),
      etapa = "Respondió módulo político",
      detalle = NA_character_,
      n = c(sum(control$drmnar_r), sum(trat$drmnar_r))
    ),
    desglose
  )
}

#' Descriptivos del instrumento y las dicotómicas (Table A16)
#'
#' Calcula por subconjunto los indicadores estructurales del experimento
#' (proporción en tratamiento `Z`, tasas de respuesta `R`, `R | Z=1`,
#' `R | Z=0`) y la media y N de cada dicotómica `drmnar_y_*` entre los
#' respondientes del módulo político.
#'
#' @param bd Snapshot con las variables de [preparar_variables_drmnar()].
#' @param subconjuntos Lista nombrada de vectores lógicos (`NULL` en un
#'   elemento = todas las observaciones).
#' @return Tibble con columnas `indicador`, `subconjunto`, `valor`, `n`.
#' @export
tabla_descriptivos_drmnar <- function(bd, subconjuntos = list("Todos" = NULL)) {
  if (!all(c("drmnar_z", "drmnar_r") %in% names(bd))) {
    stop("Corre primero preparar_variables_drmnar() sobre el snapshot.")
  }
  dicotomicas <- grep("^drmnar_y_", names(bd), value = TRUE)

  filas <- list()
  for (nombre in names(subconjuntos)) {
    filtro <- subconjuntos[[nombre]]
    sub <- if (is.null(filtro)) bd else bd[filtro, ]
    z <- sub$drmnar_z
    r <- sub$drmnar_r

    filas[[length(filas) + 1]] <- tibble::tibble(
      indicador = c("Z", "R", "R | Z=1", "R | Z=0"),
      subconjunto = nombre,
      valor = c(
        mean(z),
        mean(r),
        mean(r[z == 1]),
        mean(r[z == 0])
      ),
      n = c(length(z), length(r), sum(z == 1), sum(z == 0))
    )

    for (col in dicotomicas) {
      y <- sub[[col]]
      filas[[length(filas) + 1]] <- tibble::tibble(
        indicador = sub("^drmnar_y_", "", col),
        subconjunto = nombre,
        valor = mean(y, na.rm = TRUE),
        n = sum(!is.na(y))
      )
    }
  }
  dplyr::bind_rows(filas)
}

#' Segundo tipo de diseño: bundle DR-MNAR para el flujo de morantviz
#'
#' A partir del diseño muestral ponderado del flujo normal de encuestar
#' (raking incluido), corre el diagnóstico de no respuesta no ignorable
#' por pregunta/subconjunto y arma el bundle `diseno_drmnar` que acompaña
#' al diseño en el resto del pipeline (morantviz / morantviz_factory).
#'
#' La regla de decisión es automática (Sección 5 del Anexo Técnico:
#' IC 95% de gamma excluye 0 => DR-MNAR; en caso contrario => Raking) y
#' admite un `override` manual por pregunta del analista. Raking y DR-MNAR
#' son mutuamente excluyentes por estimando: las preguntas con decisión
#' "Raking" se estiman con el diseño tal cual (svymean), y las "DR-MNAR"
#' con [estimar_drmnar()] o con el diseño repesado de
#' [ajustar_pesos_drmnar()].
#'
#' @param diseno Objeto `survey::svydesign` del flujo de encuestar cuyas
#'   variables ya incluyen el instrumento (`drmnar_z`) y las preguntas
#'   (ver [preparar_variables_drmnar()]). Debe contener también a los
#'   desertores del filtro (no respondientes del módulo político).
#' @param preguntas Lista nombrada `list(pregunta = categoria(s))`.
#' @param covariables Covariables individuales y/o seccionales presentes
#'   en las variables del diseño.
#' @param instrumento Columna del brazo aleatorizado.
#' @param subconjuntos Lista nombrada de vectores lógicos para diagnóstico
#'   por subgrupo.
#' @param override Tibble opcional `pregunta`, `decision` (y opcionalmente
#'   `motivo`) que fuerza la decisión de esas preguntas; el diagnóstico
#'   original se conserva para auditoría.
#' @param ... Argumentos adicionales para [diagnosticar_norespuesta()].
#' @return Objeto de clase `diseno_drmnar`: lista con `diseno`,
#'   `diagnostico`, `decision` (con override aplicado), `covariables`,
#'   `instrumento`, `override` y `fecha`.
#' @export
generar_diseno_drmnar <- function(diseno, preguntas, covariables = NULL,
                                  instrumento = "drmnar_z",
                                  subconjuntos = NULL,
                                  override = NULL,
                                  ...) {
  diagnostico <- diagnosticar_norespuesta(
    diseno = diseno, preguntas = preguntas, covariables = covariables,
    instrumento = instrumento, subconjuntos = subconjuntos,
    ...
  )
  decision <- resumen_decision_norespuesta(diagnostico)

  if (!is.null(override)) {
    if (!all(c("pregunta", "decision") %in% names(override))) {
      stop("`override` requiere columnas `pregunta` y `decision`.")
    }
    for (i in seq_len(nrow(override))) {
      idx <- decision$pregunta == override$pregunta[i]
      if (!any(idx)) {
        warning(
          "El override de `", override$pregunta[i],
          "` no corresponde a ninguna pregunta diagnosticada."
        )
        next
      }
      decision$decision[idx] <- override$decision[i]
    }
  }

  structure(
    list(
      diseno = diseno,
      diagnostico = diagnostico,
      decision = decision,
      covariables = covariables,
      instrumento = instrumento,
      override = override,
      fecha = Sys.time()
    ),
    class = "diseno_drmnar"
  )
}

#' @export
print.diseno_drmnar <- function(x, ...) {
  cat("<diseno_drmnar>\n")
  cat("  Generado:", format(x$fecha), "\n")
  cat("  Instrumento:", x$instrumento, "\n")
  cat(
    "  Covariables:",
    if (is.null(x$covariables)) "(solo constante)" else
      paste(x$covariables, collapse = ", "),
    "\n"
  )
  cat("  Decisión por pregunta (Raking vs DR-MNAR):\n")
  for (i in seq_len(nrow(x$decision))) {
    cat(
      "    -", x$decision$pregunta[i], "=>", x$decision$decision[i],
      if (!is.null(x$override) &&
            x$decision$pregunta[i] %in% x$override$pregunta) {
        "[override manual]"
      } else {
        ""
      },
      "\n"
    )
  }
  invisible(x)
}
