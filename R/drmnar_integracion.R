# Integración DR-MNAR con el flujo de morantviz ---------------------------
#
# Dos vías complementarias para llevar la corrección por no respuesta no
# ignorable a morantviz (que calcula todo con survey::svymean sobre un
# `diseno`):
#
# 1. WEIGHT-SWAP (automática, morantviz sin cambios): un svydesign cuyos
#    pesos son el peso base del diseño multiplicado por la propensión
#    inversa MNAR (1/pi) de la pregunta. svymean sobre él reproduce el
#    estimador IPW-MNAR. Los errores estándar de svymean NO incorporan la
#    incertidumbre de la estimación de pi: para inferencia rigurosa usar
#    estimar_drmnar() (sándwich completo).
# 2. INYECCIÓN DE RESULTADOS (DR completo): exportar_resultado_morantviz()
#    emite el tibble en el esquema de salida de morantviz para graficarlo
#    con el mini-adaptador Graficar$inyectar_resultado().

#' Diseño repesado por la propensión MNAR de una pregunta (weight-swap)
#'
#' Estima el modelo de propensión MNAR de la pregunta con el instrumento
#' aleatorizado y devuelve un nuevo `survey::svydesign` cuyo peso es el
#' peso base multiplicado por `1/pi` en los respondientes del ítem. El
#' diseño resultante entra al flujo de morantviz SIN cambios
#' (`Graficar$new(diseno = ...)`) y sus medias ponderadas equivalen al
#' estimador IPW-MNAR (Weights-MNAR de [estimar_drmnar()]).
#'
#' El peso ajustado es específico de la pregunta: gamma_y difiere por
#' pregunta, así que no existe un único diseño corregido para todo el
#' cuestionario.
#'
#' @inheritParams estimar_drmnar
#' @return Objeto `survey::svydesign` con los pesos ajustados (la variable
#'   `peso_drmnar` queda en las variables del diseño para auditoría).
#' @examples
#' \dontrun{
#' dis_dr <- ajustar_pesos_drmnar(
#'   diseno, pregunta = "conoce_cruz", categoria = "Sí lo conoce",
#'   covariables = c("sexo", "rango_edad")
#' )
#' g <- morantviz::Graficar$new(diseno = dis_dr, ...)
#' }
#' @export
ajustar_pesos_drmnar <- function(diseno, pregunta, covariables = NULL,
                                 instrumento = "drmnar_z",
                                 respuesta_ind = NULL,
                                 categoria = NULL,
                                 gamma_inicial = 1.1,
                                 intervalo_gamma = c(-5, 8)) {
  ins <- extraer_insumos_drmnar(
    diseno = diseno, pregunta = pregunta, covariables = covariables,
    instrumento = instrumento, respuesta_ind = respuesta_ind,
    categoria = categoria, subconjunto = NULL
  )
  n <- length(ins$z)
  w_norm <- ins$w * n / sum(ins$w)
  y <- ifelse(ins$r == 1, ins$y, 0)

  nuc <- ajustar_nucleo_drmnar(
    z = ins$z, r = ins$r, y = y, X = ins$X, w = w_norm,
    cluster = ins$cluster, estrato = ins$estrato,
    gamma_inicial = gamma_inicial, intervalo_gamma = intervalo_gamma
  )
  if (!nuc$convergencia) {
    warning(
      "El modelo de propensión MNAR de `", pregunta, "` no convergió; ",
      "los pesos ajustados pueden ser inestables."
    )
  }

  # peso final = peso base del diseño x 1/pi (solo aporta en respondientes
  # del ítem; los no respondientes no entran a svymean de la pregunta)
  factor_mnar <- ifelse(ins$r == 1, as.numeric(nuc$ipw$pesos_mnar), 1)
  peso_final <- as.numeric(stats::weights(diseno)) * factor_mnar

  vars <- diseno$variables
  vars$peso_drmnar <- peso_final
  vars$.upm_drmnar <- diseno$cluster[[1]]
  vars$.estrato_drmnar <- if (!is.null(diseno$strata)) {
    diseno$strata[[1]]
  } else {
    1L
  }

  survey::svydesign(
    ids = ~.upm_drmnar,
    strata = ~.estrato_drmnar,
    weights = ~peso_drmnar,
    data = vars,
    nest = TRUE
  )
}

#' Exporta una estimación DR-MNAR al esquema de salida de morantviz
#'
#' Convierte el tibble de [estimar_drmnar()] al formato que producen los
#' métodos de conteo de morantviz (`respuesta`, `media`, `ee`, `inf`,
#' `sup`, `codigo`) para inyectarlo con `Graficar$inyectar_resultado()` y
#' reutilizar sus gráficas con el estimador doblemente robusto completo.
#'
#' @param estimacion Tibble de [estimar_drmnar()].
#' @param modelo Estimador a exportar (default `"DR-MNAR"`).
#' @return Tibble de una fila con el esquema de morantviz.
#' @export
exportar_resultado_morantviz <- function(estimacion, modelo = "DR-MNAR") {
  fila <- estimacion[estimacion$modelo == modelo, ]
  if (nrow(fila) == 0) {
    stop(
      "El modelo `", modelo, "` no está en la estimación. Disponibles: ",
      paste(unique(estimacion$modelo), collapse = ", ")
    )
  }
  tibble::tibble(
    respuesta = ifelse(is.na(fila$categoria), fila$pregunta, fila$categoria),
    media = fila$est,
    ee = fila$ee,
    inf = fila$inf,
    sup = fila$sup,
    codigo = fila$pregunta
  )
}

#' Diseño a usar para una pregunta según la decisión del bundle
#'
#' Implementa el árbol de decisión operativo sobre el bundle
#' [generar_diseno_drmnar()]: si la decisión de la pregunta es "Raking"
#' devuelve el diseño original intacto (raking y DR-MNAR son mutuamente
#' excluyentes por estimando); si es "DR-MNAR" devuelve el diseño
#' repesado por [ajustar_pesos_drmnar()].
#'
#' @param bundle Objeto `diseno_drmnar` de [generar_diseno_drmnar()].
#' @param pregunta Pregunta a estimar.
#' @param categoria Categoría que define y = 1 (para el repesado).
#' @param ... Argumentos adicionales para [ajustar_pesos_drmnar()].
#' @return Objeto `survey::svydesign`.
#' @export
diseno_para_pregunta <- function(bundle, pregunta, categoria = NULL, ...) {
  if (!inherits(bundle, "diseno_drmnar")) {
    stop("`bundle` debe ser un objeto de generar_diseno_drmnar().")
  }
  idx <- bundle$decision$pregunta == pregunta
  if (!any(idx)) {
    stop(
      "La pregunta `", pregunta, "` no está en la decisión del bundle. ",
      "Diagnosticadas: ",
      paste(bundle$decision$pregunta, collapse = ", ")
    )
  }
  decision <- bundle$decision$decision[idx][1]
  if (decision == "DR-MNAR") {
    ajustar_pesos_drmnar(
      diseno = bundle$diseno, pregunta = pregunta,
      covariables = bundle$covariables,
      instrumento = bundle$instrumento,
      categoria = categoria, ...
    )
  } else {
    bundle$diseno
  }
}
