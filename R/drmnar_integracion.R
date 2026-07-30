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

#' Diseño de réplicas DR-MNAR con varianza correcta (weight-swap riguroso)
#'
#' Versión de [ajustar_pesos_drmnar()] cuyo `survey::svymean`/`svyby`
#' **sí** incorpora la incertidumbre de la propensión estimada `pi_hat`.
#'
#' El weight-swap ingenuo de [ajustar_pesos_drmnar()] entrega un
#' `svydesign` cuyos pesos (diseño x `1/pi_hat`) `survey` trata como
#' **fijos**: los errores estándar de `svymean` salen demasiado angostos
#' porque ignoran que `pi_hat` es estimado. Aquí, en cambio, se construye
#' un `survey::svrepdesign` con réplicas de bootstrap por conglomerado
#' (Rao-Wu-Yue): en cada réplica se remuestrean las UPM dentro de estrato
#' y se **re-estima `pi_hat` con los pesos de la réplica**, de modo que la
#' dispersión entre réplicas captura la varianza del muestreo de UPM y la
#' de la propensión. `survey::svymean` sobre el resultado la propaga
#' automáticamente, así que morantviz reporta intervalos correctos sin
#' cambios. Dos límites del alcance: los factores de rake del diseño de
#' entrada quedan **fijos** (no se re-calibran por réplica), y los
#' estratos con una sola UPM no aportan varianza entre réplicas (la
#' función avisa cuando los hay).
#'
#' Igual que en [ajustar_pesos_drmnar()], el diseño devuelto es
#' **específico de `pregunta`**: sus pesos (base y de réplica) llevan el
#' factor MNAR de esa pregunta. No lo uses para estimar otras preguntas —
#' construye un diseño de réplicas por pregunta con decisión DR-MNAR.
#'
#' Es la vía recomendada cuando la decisión de la pregunta es DR-MNAR y se
#' quieren **intervalos publicables** con el flujo de morantviz. El costo
#' es tiempo de cómputo (una estimación de propensión por réplica).
#'
#' En el diseño devuelto, la columna `pregunta` viene **enmascarada
#' (`NA`) donde la respuesta por protocolo es `r = 0`**: un desertor del
#' filtro pudo dejar valores en campo (batería corta) pero no cuenta como
#' respuesta del módulo político; sin enmascarar, `svymean` lo mezclaría
#' con peso base y el punto quedaría mal. Así el diseño es seguro para
#' `svymean(~pregunta, na.rm = TRUE)` tal cual lo usa morantviz.
#'
#' @inheritParams ajustar_pesos_drmnar
#' @param n_replicas Número de réplicas de bootstrap (default 200; más
#'   réplicas = EE más estable, más cómputo).
#' @param semilla Semilla para el remuestreo (reproducibilidad).
#' @return Objeto `survey::svrepdesign` (tipo bootstrap) con los pesos
#'   DR-MNAR en la réplica base y `n_replicas` columnas de réplica.
#' @examples
#' \dontrun{
#' rep <- disenar_replicas_drmnar(
#'   diseno, pregunta = "conoce_cruz", categoria = "Sí lo conoce",
#'   covariables = c("sexo", "rango_edad")
#' )
#' g <- morantviz::Graficar$new(diseno = rep, ...)  # EE correctos
#' }
#' @export
disenar_replicas_drmnar <- function(diseno, pregunta, covariables = NULL,
                                    instrumento = "drmnar_z",
                                    respuesta_ind = NULL,
                                    categoria = NULL,
                                    n_replicas = 200,
                                    gamma_inicial = 1.1,
                                    intervalo_gamma = c(-5, 8),
                                    semilla = 1) {
  ins <- extraer_insumos_drmnar(
    diseno = diseno, pregunta = pregunta, covariables = covariables,
    instrumento = instrumento, respuesta_ind = respuesta_ind,
    categoria = categoria, subconjunto = NULL
  )
  n <- length(ins$z)
  w_base <- as.numeric(stats::weights(diseno))
  y <- ifelse(ins$r == 1, ins$y, 0)
  cluster <- if (!is.null(ins$cluster)) as.character(ins$cluster) else as.character(seq_len(n))
  estrato <- if (!is.null(ins$estrato)) as.character(ins$estrato) else rep("1", n)
  # ids de UPM únicos ENTRE estratos: si dos estratos comparten etiqueta de
  # UPM, el split() de abajo mezclaría sus filas
  cluster <- paste(estrato, cluster, sep = "||")

  # propensión MNAR sobre un subconjunto de filas (una réplica), ajustada
  # con los pesos DE LA RÉPLICA (w x multiplicador RWY): el refit debe ver
  # la misma muestra ponderada que el promedio, si no la varianza de pi_hat
  # se captura solo en parte y el EE queda angosto. Devuelve NULL si el
  # núcleo no ajusta o no converge.
  pesos_mnar_en <- function(filas, mult = NULL) {
    w_sub <- ins$w[filas]
    if (!is.null(mult)) w_sub <- w_sub * mult[filas]
    w_sub <- w_sub * length(filas) / sum(w_sub)
    nuc <- tryCatch(
      ajustar_nucleo_drmnar(
        z = ins$z[filas], r = ins$r[filas], y = y[filas],
        X = ins$X[filas, , drop = FALSE], w = w_sub,
        cluster = NULL, estrato = NULL,
        gamma_inicial = gamma_inicial, intervalo_gamma = intervalo_gamma
      ),
      error = function(e) NULL
    )
    if (is.null(nuc) || !isTRUE(nuc$convergencia)) {
      return(NULL)
    }
    ifelse(ins$r[filas] == 1, as.numeric(nuc$ipw$pesos_mnar), 1)
  }

  # réplica base (todas las filas): si el núcleo no ajusta aquí, el diseño
  # entero saldría etiquetado DR-MNAR sin corrección — mejor abortar
  factor_base <- pesos_mnar_en(seq_len(n))
  if (is.null(factor_base)) {
    stop(
      "El modelo de propensión MNAR de `", pregunta, "` no ajustó o no ",
      "convergió sobre la muestra completa; no se puede construir el ",
      "diseño de réplicas. Revisa covariables (colinealidad, celdas ",
      "unánimes) o `intervalo_gamma`.",
      call. = FALSE
    )
  }
  peso_base <- w_base * factor_base

  # bootstrap por conglomerado dentro de estrato (Rao-Wu-Yue): en cada
  # réplica se remuestrean nh-1 UPM por estrato con reescalado nh/(nh-1)
  set.seed(semilla)
  filas_por_upm <- split(seq_len(n), cluster)
  upm_de_estrato <- tapply(cluster, estrato, function(x) unique(x))
  estratos_u <- names(upm_de_estrato)

  solitarios <- sum(lengths(upm_de_estrato) <= 1)
  if (solitarios > 0) {
    warning(
      solitarios, " estrato(s) con una sola UPM: contribuyen varianza ",
      "cero entre réplicas y el EE puede quedar subestimado. Considera ",
      "colapsar estratos.",
      call. = FALSE
    )
  }

  rep_weights <- matrix(0, nrow = n, ncol = n_replicas)
  fallos <- 0L
  for (b in seq_len(n_replicas)) {
    mult <- numeric(n) # multiplicador de bootstrap por fila (0 = fuera)
    for (h in estratos_u) {
      upms <- upm_de_estrato[[h]]
      nh <- length(upms)
      if (nh <= 1) {
        for (u in upms) mult[filas_por_upm[[u]]] <- 1
      } else {
        draws <- sample(upms, nh - 1, replace = TRUE)
        tab <- table(factor(draws, levels = upms)) * nh / (nh - 1)
        for (u in upms) mult[filas_por_upm[[u]]] <- tab[[u]]
      }
    }
    filas_b <- which(mult > 0)
    factor_b <- numeric(n)
    fb <- pesos_mnar_en(filas_b, mult)
    if (is.null(fb)) {
      # fallback: esa réplica queda sin corrección MNAR (se cuenta y avisa)
      fallos <- fallos + 1L
      fb <- rep(1, length(filas_b))
    }
    factor_b[filas_b] <- fb
    rep_weights[, b] <- w_base * factor_b * mult
  }
  if (fallos > 0) {
    warning(
      fallos, " de ", n_replicas, " réplicas no ajustaron el modelo de ",
      "propensión y quedaron sin corrección MNAR; con muchas fallas el ",
      "EE deja de ser confiable.",
      call. = FALSE
    )
  }

  vars <- diseno$variables
  vars$peso_drmnar <- peso_base
  # protocolo: r = 0 no es respuesta del módulo aunque la columna traiga
  # valor (desertor con batería corta, o simulación que conserva la verdad)
  if (pregunta %in% names(vars)) {
    vars[[pregunta]][which(ins$r == 0)] <- NA
  }

  survey::svrepdesign(
    data = vars,
    weights = ~peso_drmnar,
    repweights = rep_weights,
    type = "bootstrap",
    combined.weights = TRUE
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
