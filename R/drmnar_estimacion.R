# Estimación DR-MNAR sobre un diseño muestral de encuestar ---------------
#
# API pública del estimador doblemente robusto bajo no respuesta no
# ignorable. Consume el objeto `survey::svydesign` del flujo de encuestar
# (pesos de diseño + UPM + estratos) y produce la comparación de los 7
# estimadores de la Figura 1 de Bailey (2023): Observado, ponderado,
# imputación y doblemente robusto, en versiones MAR (gamma = 0) y MNAR
# (gamma estimado con el instrumento aleatorizado).

#' Extrae los insumos del motor DR-MNAR desde un diseño muestral
#'
#' Valida el contrato de datos (instrumento, pregunta, covariables),
#' dicotomiza la pregunta, deriva el indicador de respuesta por ítem y
#' recorta al subconjunto solicitado.
#'
#' @keywords internal
#' @noRd
extraer_insumos_drmnar <- function(diseno, pregunta, covariables,
                                   instrumento, respuesta_ind,
                                   categoria, subconjunto) {
  vars <- diseno$variables

  if (!instrumento %in% names(vars)) {
    stop(
      "No existe la columna de instrumento aleatorizado `", instrumento,
      "` en el diseño. El método DR-MNAR requiere el brazo experimental ",
      "(protocolo de la Sección 2 del Anexo Técnico)."
    )
  }
  if (!pregunta %in% names(vars)) {
    stop("No existe la pregunta `", pregunta, "` en el diseño.")
  }

  y_cruda <- vars[[pregunta]]
  if (is.null(categoria)) {
    valores <- unique(y_cruda[!is.na(y_cruda)])
    if (is.numeric(y_cruda) && all(valores %in% c(0, 1))) {
      y <- as.numeric(y_cruda)
    } else {
      stop(
        "La pregunta `", pregunta, "` no es dicotómica 0/1: especifica ",
        "`categoria` (el valor o valores que definen y = 1, p. ej. ",
        '"Sí lo conoce").'
      )
    }
  } else {
    y <- as.numeric(y_cruda %in% categoria)
  }

  # indicador de respuesta por ítem: si no se especifica columna, no
  # respondió el ítem = NA en la pregunta
  if (is.null(respuesta_ind)) {
    r <- as.numeric(!is.na(y_cruda))
  } else {
    if (!respuesta_ind %in% names(vars)) {
      stop("No existe la columna de respuesta `", respuesta_ind, "`.")
    }
    r <- as.numeric(vars[[respuesta_ind]])
    sin_item <- r == 1 & is.na(y_cruda)
    if (any(sin_item)) {
      message(
        sum(sin_item), " registros marcados como respuesta pero sin valor ",
        "en `", pregunta, "`; se tratan como no respuesta del ítem."
      )
      r[sin_item] <- 0
    }
  }

  z <- as.numeric(vars[[instrumento]])

  # matriz de covariables con constante; model.matrix expande categóricas
  if (is.null(covariables)) {
    X <- matrix(1, nrow = nrow(vars), ncol = 1)
  } else {
    faltan <- setdiff(covariables, names(vars))
    if (length(faltan) > 0) {
      stop("Covariables inexistentes: ", paste(faltan, collapse = ", "))
    }
    bd_cov <- vars[, covariables, drop = FALSE]
    n_na <- sum(!stats::complete.cases(bd_cov))
    if (n_na > 0) {
      stop(
        n_na, " registros tienen NA en las covariables (",
        paste(covariables, collapse = ", "), "). El protocolo de rescate ",
        "mínimo exige demográficos del 100% de los contactos; imputa o ",
        "retira esas covariables."
      )
    }
    X <- stats::model.matrix(~., data = bd_cov)
  }

  w <- as.numeric(stats::weights(diseno))
  cluster <- as.character(diseno$cluster[[1]])
  estrato <- if (!is.null(diseno$strata)) {
    as.character(diseno$strata[[1]])
  } else {
    NULL
  }

  if (!is.null(subconjunto)) {
    if (length(subconjunto) != nrow(vars)) {
      stop("`subconjunto` debe ser un vector lógico de longitud ", nrow(vars), ".")
    }
    sel <- which(subconjunto)
    if (length(sel) == 0) {
      stop("El subconjunto no contiene observaciones.")
    }
    y <- y[sel]
    r <- r[sel]
    z <- z[sel]
    X <- X[sel, , drop = FALSE]
    w <- w[sel]
    cluster <- cluster[sel]
    estrato <- estrato[sel]
  }

  list(z = z, r = r, y = y, X = X, w = w, cluster = cluster, estrato = estrato)
}

# Momentos de los estimadores MAR (gamma = 0) ----------------------------

#' @keywords internal
#' @noRd
momentos_hajek <- function(theta, r, y, w) {
  matrix(w * r * (y - theta), nrow = 1)
}

#' @keywords internal
#' @noRd
momentos_imp_mar <- function(theta, r, y, ZX_int, w) {
  k <- ncol(ZX_int)
  delta <- theta[seq_len(k)]
  mu <- theta[k + 1]
  pr_y <- as.numeric(expit(ZX_int %*% delta))
  rbind(
    t(ZX_int * (w * r * (y - pr_y))),
    matrix(w * (r * y + (1 - r) * pr_y - mu), nrow = 1)
  )
}

#' @keywords internal
#' @noRd
momentos_dr_mar <- function(theta, r, y, ZX, ZX_int, w) {
  k1 <- ncol(ZX)
  k2 <- ncol(ZX_int)
  alpha <- theta[seq_len(k1)]
  delta <- theta[k1 + seq_len(k2)]
  mu <- theta[k1 + k2 + 1]
  pi_r <- as.numeric(expit(ZX %*% alpha))
  pr_y <- as.numeric(expit(ZX_int %*% delta))
  rbind(
    t(ZX * (w * (r - pi_r))),
    t(ZX_int * (w * r * (y - pr_y))),
    matrix(w * (r / pi_r * (y - pr_y) + pr_y - mu), nrow = 1)
  )
}

#' Estimación doblemente robusta bajo no respuesta no ignorable (DR-MNAR)
#'
#' Calcula la comparación completa de estimadores de la Figura 1 de Bailey
#' (2023) para una pregunta dicotomizada de la encuesta, incorporando los
#' pesos del diseño muestral complejo y la varianza sándwich agrupada por
#' UPM y estrato:
#'
#' * `Observado`: media simple de los respondientes (sin pesos).
#' * `Weights-MAR`: media ponderada por el diseño (equivale a
#'   `survey::svymean` con el diseño rakeado — el flujo actual de Morant).
#' * `Imput-MAR` / `DR-MAR`: imputación y doblemente robusto asumiendo
#'   ignorabilidad (gamma = 0).
#' * `Weights-MNAR` / `Imput-MNAR` / `DR-MNAR`: los tres estimadores de
#'   Bailey con gamma estimado mediante el instrumento aleatorizado.
#'
#' El peso analítico de los estimadores MNAR es el peso base del diseño
#' multiplicado por la propensión inversa `1/pi_i`, y en el DR el peso se
#' aplica sobre el residuo `(y - y_imputada)` (Sección 6 del Anexo Técnico).
#'
#' @param diseno Objeto `survey::svydesign` del flujo de encuestar (con
#'   pesos, UPM y estratos declarados).
#' @param pregunta Nombre de la variable de opinión a estimar.
#' @param covariables Vector de nombres de covariables (demográficas y
#'   contextuales censales/electorales por sección). `NULL` usa solo la
#'   constante.
#' @param instrumento Nombre de la columna con el brazo del instrumento
#'   aleatorizado (0 = control, 1 = tratamiento).
#' @param respuesta_ind Nombre de la columna con el indicador de respuesta
#'   del ítem; `NULL` lo deriva de los NA de la pregunta.
#' @param categoria Valor (o valores) de la pregunta que definen `y = 1`,
#'   p. ej. `"Sí lo conoce"`. Innecesario si la pregunta ya es 0/1.
#' @param subconjunto Vector lógico para estimar sobre un subgrupo (puede
#'   haber MNAR por subgrupo aunque no lo haya globalmente).
#' @param nombre_subconjunto Etiqueta del subgrupo en la salida.
#' @param gamma_inicial Valor inicial de gamma para el solucionador.
#' @param intervalo_gamma Intervalo de búsqueda de gamma.
#' @param nivel_confianza Nivel de los intervalos (default 95%).
#' @return Tibble con columnas `modelo`, `est`, `ee`, `inf`, `sup`,
#'   `gamma_y`, `ee_gamma_y`, `pregunta`, `categoria`, `subconjunto`,
#'   `n`, `convergencia`.
#' @examples
#' \dontrun{
#' estimar_drmnar(
#'   diseno = encuesta$muestra$diseno,
#'   pregunta = "conoce_cruz",
#'   covariables = c("sexo", "rango_edad", "escolaridad_prom"),
#'   categoria = "Sí lo conoce"
#' )
#' }
#' @export
estimar_drmnar <- function(diseno, pregunta, covariables = NULL,
                           instrumento = "drmnar_z",
                           respuesta_ind = NULL,
                           categoria = NULL,
                           subconjunto = NULL,
                           nombre_subconjunto = "Estado",
                           gamma_inicial = 1.1,
                           intervalo_gamma = c(-5, 8),
                           nivel_confianza = 0.95) {
  ins <- extraer_insumos_drmnar(
    diseno = diseno, pregunta = pregunta, covariables = covariables,
    instrumento = instrumento, respuesta_ind = respuesta_ind,
    categoria = categoria, subconjunto = subconjunto
  )
  z_crit <- stats::qnorm(1 - (1 - nivel_confianza) / 2)
  n <- length(ins$z)
  w <- ins$w * n / sum(ins$w)
  r <- ins$r
  y <- ifelse(r == 1, ins$y, 0)

  # ---- estimadores MNAR (núcleo de Bailey con pesos de diseño) ----
  nuc <- ajustar_nucleo_drmnar(
    z = ins$z, r = r, y = y, X = ins$X, w = w,
    cluster = ins$cluster, estrato = ins$estrato,
    gamma_inicial = gamma_inicial, intervalo_gamma = intervalo_gamma
  )

  # ---- estimadores MAR (gamma = 0) ----
  X <- ins$X
  k_x <- ncol(X)
  ZX <- cbind(X[, 1], ins$z, X[, -1, drop = FALSE])
  ZX_int <- if (k_x > 1) cbind(ZX, ins$z * X[, -1, drop = FALSE]) else ZX

  # Weights-MAR: media ponderada por el diseño (svymean + rake)
  mu_wmar <- sum(w * r * y) / sum(w * r)
  se_wmar <- calcular_sandwich(
    function(th) momentos_hajek(th, r, y, w) / mean(w * r),
    theta = mu_wmar, cluster = ins$cluster, estrato = ins$estrato
  )

  # Imput-MAR: imputación del resultado con gamma = 0
  obs <- r == 1
  pcc <- ajustar_logit_ponderado(ZX_int[obs, , drop = FALSE], y[obs], w[obs])
  delta <- pcc$coefficients
  pr_y <- as.numeric(expit(ZX_int %*% delta))
  mu_imar <- sum(w * (r * y + (1 - r) * pr_y)) / sum(w)
  se_imar <- calcular_sandwich(
    function(th) momentos_imp_mar(th, r, y, ZX_int, w),
    theta = c(delta, mu_imar), cluster = ins$cluster, estrato = ins$estrato
  )
  se_imar <- se_imar[length(se_imar)]

  # DR-MAR: doblemente robusto con propensión MAR (sin y)
  prop_mar <- ajustar_logit_ponderado(ZX, r, w)
  alpha_mar <- prop_mar$coefficients
  pi_mar <- as.numeric(expit(ZX %*% alpha_mar))
  mu_drmar <- sum(w * (r / pi_mar * (y - pr_y) + pr_y)) / sum(w)
  se_drmar <- calcular_sandwich(
    function(th) momentos_dr_mar(th, r, y, ZX, ZX_int, w),
    theta = c(alpha_mar, delta, mu_drmar),
    cluster = ins$cluster, estrato = ins$estrato
  )
  se_drmar <- se_drmar[length(se_drmar)]

  # Observado: media simple de los respondientes SIN pesos (referencia
  # más ingenua, como la fila "Observed" de Bailey). El núcleo devuelve la
  # versión ponderada, que aquí corresponde a Weights-MAR.
  mu_obs <- mean(y[r == 1])
  se_obs <- sqrt(mu_obs * (1 - mu_obs) / sum(r == 1))

  categoria_txt <- if (is.null(categoria)) {
    NA_character_
  } else {
    paste(categoria, collapse = " | ")
  }

  fila <- function(modelo, est, ee, gamma = NA_real_, ee_gamma = NA_real_) {
    tibble::tibble(
      modelo = modelo,
      est = as.numeric(est),
      ee = as.numeric(ee),
      inf = as.numeric(est) - z_crit * as.numeric(ee),
      sup = as.numeric(est) + z_crit * as.numeric(ee),
      gamma_y = as.numeric(gamma),
      ee_gamma_y = as.numeric(ee_gamma)
    )
  }

  dplyr::bind_rows(
    fila("Observado", mu_obs, se_obs),
    fila("Weights-MAR", mu_wmar, se_wmar),
    fila("Imput-MAR", mu_imar, se_imar),
    fila("DR-MAR", mu_drmar, se_drmar),
    fila("Weights-MNAR", nuc$ipw$y_est, nuc$ipw$se_y_est,
         nuc$ipw$gamma_y, nuc$ipw$se_gamma_y),
    fila("Imput-MNAR", nuc$imp$y_est, nuc$imp$se_y_est,
         nuc$imp$gamma_y, nuc$imp$se_gamma_y),
    fila("DR-MNAR", nuc$dr$y_est, nuc$dr$se_y_est,
         nuc$dr$gamma_y, nuc$dr$se_gamma_y)
  ) |>
    dplyr::mutate(
      pregunta = pregunta,
      categoria = categoria_txt,
      subconjunto = nombre_subconjunto,
      n = n,
      convergencia = nuc$convergencia
    )
}
