# Núcleo DR-MNAR ---------------------------------------------------------
#
# Port de las ecuaciones de estimación de Bailey (2023) "Countering
# Non-Ignorable Nonresponse in Survey Models with Randomized Response
# Instruments and Doubly Robust Estimation" (Political Analysis), a su vez
# basadas en Sun et al. (2018, Statistica Sinica), generalizadas en dos
# direcciones para el flujo de encuestar:
#
#   1. Pesos de diseño: cada contribución de momento se pondera por w_i
#      (peso base del diseño muestral complejo). Con w_i = 1 se reproduce
#      exactamente el estimador iid de Bailey (ver test-drmnar-replicacion.R).
#   2. Varianza sándwich clusterizada: el "meat" se agrupa por UPM y
#      estrato en lugar de asumir observaciones independientes.
#
# Notación (idéntica al Anexo Técnico DR-MNAR):
#   z = brazo del instrumento aleatorizado, r = indicador de respuesta,
#   y = opinión dicotómica (solo observada cuando r = 1),
#   gamma_y = parámetro crítico de no ignorabilidad.
#
# Todas las funciones de este archivo son internas (no exportadas).

#' Función logística inversa
#' @keywords internal
#' @noRd
expit <- function(x) {
  stats::plogis(x)
}

#' Probabilidad del resultado entre los no respondientes
#'
#' Inclinación odds-ratio de la probabilidad estimada de los respondientes
#' por el parámetro de selección gamma (ecuación de imputación de la
#' Sección 4 del Anexo Técnico; `p.unobs` en Bailey):
#' P(Y = y | R = 0, X) = exp(-gamma * y) P(Y = y | X) / suma_y' exp(-gamma y') P(Y = y'|X)
#'
#' @param gamma Parámetro de selección por no respuesta.
#' @param y Valor del resultado (0 o 1) al que se evalúa la probabilidad.
#' @param pr_y Vector de probabilidades P(Y = 1 | X) de los respondientes.
#' @keywords internal
#' @noRd
p_no_obs <- function(gamma, y, pr_y) {
  denom <- exp(-gamma) * pr_y + (1 - pr_y)
  y * (exp(-gamma) * pr_y / denom) + (1 - y) * ((1 - pr_y) / denom)
}

#' Ecuaciones de momento del modelo de propensión MNAR (IPW)
#'
#' Port ponderado de `ipw.gen` de Bailey. Las primeras k-1 ecuaciones
#' igualan E[w (R/pi - 1) h(Z, X)] = 0; la última identifica gamma_y con el
#' instrumento: E[w (R Y / pi)(Z - P(Z|X))] = 0. Para r = 0 el término
#' R/pi - 1 = -1 no depende de y, por lo que el valor de y de los no
#' respondientes es irrelevante.
#'
#' @param par Vector de parámetros del modelo de propensión (el último es gamma_y).
#' @param r Indicador de respuesta.
#' @param ZXY Matriz de diseño `[1, z, X, y]`.
#' @param pz_fit Probabilidades ajustadas P(Z = 1 | X).
#' @param w Pesos de diseño.
#' @keywords internal
#' @noRd
ec_ipw <- function(par, r, ZXY, pz_fit, w) {
  k <- ncol(ZXY)
  pr_r <- as.numeric(expit(ZXY %*% par))
  h <- numeric(k)
  base <- r / pr_r - 1
  for (j in seq_len(k - 1)) {
    h[j] <- sum(w * base * ZXY[, j])
  }
  h[k] <- sum(w * (r * ZXY[, k] / pr_r) * (ZXY[, 2] - pz_fit))
  h
}

#' Ecuación de momento del estimador por imputación (identifica gamma_y)
#'
#' Port ponderado de `imp` de Bailey: E[w (Z - P(Z|X)) ((1-R) p0(gamma) + R Y)] = 0,
#' donde p0 es la probabilidad inclinada de los no respondientes.
#'
#' @keywords internal
#' @noRd
ec_imp <- function(gamma, z, r, y, pr_y, pz_fit, w) {
  sum(w * (z - pz_fit) * ((1 - r) * p_no_obs(gamma, 1, pr_y) + r * y))
}

#' Ecuación de momento del estimador doblemente robusto
#'
#' Port ponderado de `dr` de Bailey. El peso de propensión inversa
#' multiplica al residuo (Y - p0), no a Y crudo:
#' E[w (Z - P(Z|X)) (R/pi (Y - p0) + p0)] = 0.
#'
#' @param dr_arg Índice lineal del modelo de propensión sin el término de y
#'   (`ZX %*% alpha_ipw`).
#' @keywords internal
#' @noRd
ec_dr <- function(gamma, z, r, y, pr_y, pz_fit, dr_arg, w) {
  p0 <- p_no_obs(gamma, 1, pr_y)
  pr_r <- as.numeric(expit(dr_arg + gamma * y))
  sum(w * (z - pz_fit) * (r / pr_r * (y - p0) + p0))
}

# Errores estándar sándwich ----------------------------------------------

#' Varianza sándwich de ecuaciones de estimación apiladas
#'
#' Calcula los errores estándar asintóticos de un estimador definido por
#' ecuaciones de momento apiladas: Var = B^{-1} Meat B^{-T} / n, donde
#' B = jacobiano de los momentos promedio y Meat es el producto exterior de
#' las contribuciones.
#'
#' Con `cluster` (y opcionalmente `estrato`) las contribuciones se agregan
#' por UPM antes del producto exterior, replicando el estimador de varianza
#' por linearización de los diseños complejos: ignorar la estructura de
#' conglomerados subestimaría los errores estándar.
#'
#' @param momentos_fn Función `theta -> matriz p x n` de contribuciones de
#'   momento por observación (ya multiplicadas por los pesos de diseño).
#' @param theta Vector de parámetros en el que se evalúa la varianza.
#' @param cluster Identificador de UPM por observación (NULL = iid).
#' @param estrato Identificador de estrato por observación; activa el
#'   centrado dentro de estrato con corrección n_h / (n_h - 1).
#' @return Vector de errores estándar (uno por parámetro).
#' @keywords internal
#' @noRd
calcular_sandwich <- function(momentos_fn, theta, cluster = NULL, estrato = NULL) {
  m <- momentos_fn(theta)
  if (is.null(dim(m))) {
    m <- matrix(m, nrow = 1)
  }
  n <- ncol(m)

  momentos_media <- function(th) {
    mm <- momentos_fn(th)
    if (is.null(dim(mm))) {
      mm <- matrix(mm, nrow = 1)
    }
    rowSums(mm) / n
  }
  dM <- numDeriv::jacobian(momentos_media, theta)

  if (is.null(cluster)) {
    carne <- tcrossprod(m) / n
  } else if (is.null(estrato)) {
    # CR0: sumar contribuciones por cluster y producto exterior
    mg <- rowsum(t(m), group = as.character(cluster)) # G x p
    carne <- crossprod(mg) / n
  } else {
    # cluster anidado en estrato: centrado dentro de estrato con
    # corrección de grados de libertad n_h / (n_h - 1)
    id <- paste(as.character(estrato), as.character(cluster), sep = "\r")
    mg <- rowsum(t(m), group = id) # G x p
    estrato_g <- vapply(
      strsplit(rownames(mg), "\r", fixed = TRUE),
      function(s) s[[1]],
      character(1)
    )
    p <- nrow(m)
    carne <- matrix(0, p, p)
    for (h in unique(estrato_g)) {
      mgh <- mg[estrato_g == h, , drop = FALSE]
      n_h <- nrow(mgh)
      centrado <- sweep(mgh, 2, colMeans(mgh))
      factor_h <- if (n_h > 1) n_h / (n_h - 1) else 1
      carne <- carne + factor_h * crossprod(centrado)
    }
    carne <- carne / n
  }

  pan <- solve(dM)
  var_theta <- pan %*% carne %*% t(pan) / n
  sqrt(pmax(diag(var_theta), 0))
}

# Contribuciones de momento por observación (para el sándwich) -----------

#' Momentos apilados del estimador IPW-MNAR
#'
#' theta = c(beta_pz, par_ipw, mu). Port ponderado de `mm.ipw_gen` de Bailey.
#' @keywords internal
#' @noRd
momentos_ipw <- function(theta, z, r, y, X, ZX, ZXY, w) {
  k_x <- ncol(X)
  k_zxy <- ncol(ZXY)
  beta_pz <- theta[seq_len(k_x)]
  par_ipw <- theta[(k_x + 1):(k_x + k_zxy)]
  mu <- theta[length(theta)]

  z_menos <- z - as.numeric(expit(X %*% beta_pz))
  pr_r <- as.numeric(expit(ZXY %*% par_ipw))
  base_r <- r / pr_r - 1

  rbind(
    t(X * (w * z_menos)),
    t(ZX * (w * base_r)),
    matrix(w * (r * y / pr_r) * z_menos, nrow = 1),
    matrix(w * (r * y / pr_r - mu), nrow = 1)
  )
}

#' Momentos apilados del estimador por imputación MNAR
#'
#' theta = c(beta_pz, delta_pcc, gamma, mu). Port ponderado de `mm.imp_gen`.
#' @keywords internal
#' @noRd
momentos_imp <- function(theta, z, r, y, X, ZX_int, w) {
  k_x <- ncol(X)
  k_int <- ncol(ZX_int)
  beta_pz <- theta[seq_len(k_x)]
  delta <- theta[(k_x + 1):(k_x + k_int)]
  gamma <- theta[k_x + k_int + 1]
  mu <- theta[length(theta)]

  z_menos <- z - as.numeric(expit(X %*% beta_pz))
  pr_y <- as.numeric(expit(ZX_int %*% delta))
  p0 <- p_no_obs(gamma, 1, pr_y)

  rbind(
    t(X * (w * z_menos)),
    t(ZX_int * (w * r * (y - pr_y))),
    matrix(w * z_menos * ((1 - r) * p0 + r * y), nrow = 1),
    matrix(w * (r * y + (1 - r) * p0 - mu), nrow = 1)
  )
}

#' Momentos apilados del estimador doblemente robusto MNAR
#'
#' theta = c(beta_pz, alpha_ipw, gamma, delta_pcc, mu). Port ponderado de
#' `mm.dr_gen` de Bailey, corrigiendo la indexación de los scores del
#' modelo de imputación en el "meat" (la réplica original reutiliza por
#' error los índices del bloque de propensión).
#' @keywords internal
#' @noRd
momentos_dr <- function(theta, z, r, y, X, ZX, ZXY, ZX_int, w) {
  k_x <- ncol(X)
  k_zx <- ncol(ZX)
  k_int <- ncol(ZX_int)
  beta_pz <- theta[seq_len(k_x)]
  par_prop <- theta[(k_x + 1):(k_x + k_zx + 1)] # alpha_ipw y gamma
  gamma <- par_prop[k_zx + 1]
  delta <- theta[(k_x + k_zx + 1 + 1):(k_x + k_zx + 1 + k_int)]
  mu <- theta[length(theta)]

  z_menos <- z - as.numeric(expit(X %*% beta_pz))
  pr_r <- as.numeric(expit(ZXY %*% par_prop))
  base_r <- r / pr_r - 1
  pr_y <- as.numeric(expit(ZX_int %*% delta))
  p0 <- p_no_obs(gamma, 1, pr_y)
  termino_dr <- r / pr_r * (y - p0) + p0

  rbind(
    t(X * (w * z_menos)),
    t(ZX * (w * base_r)),
    matrix(w * termino_dr * z_menos, nrow = 1),
    t(ZX_int * (w * r * (y - pr_y))),
    matrix(w * (termino_dr - mu), nrow = 1)
  )
}

# Motor de estimación -----------------------------------------------------

#' Ajuste de regresión logística ponderada por matriz de diseño
#'
#' Envoltura de `stats::glm.fit` con familia quasibinomial (idéntica a la
#' binomial en coeficientes, sin warnings por pesos no enteros).
#' @keywords internal
#' @noRd
ajustar_logit_ponderado <- function(X, y, w) {
  ajuste <- suppressWarnings(
    stats::glm.fit(x = X, y = y, weights = w, family = stats::quasibinomial())
  )
  coefs <- ajuste$coefficients
  if (anyNA(coefs)) {
    stop(
      "Covariables no estimables en el modelo logístico (colinealidad o ",
      "celdas unánimes). Retira las covariables sin variación en este ",
      "subconjunto antes de estimar."
    )
  }
  ajuste
}

#' Motor DR-MNAR: propensión IPW, imputación y estimador doblemente robusto
#'
#' Ejecuta los tres pasos de Bailey sobre vectores ya preparados,
#' con pesos de diseño opcionales y varianza sándwich clusterizada:
#'
#' 1. Modelo del instrumento P(Z = 1 | X) (logit ponderado).
#' 2. IPW-MNAR: resuelve las ecuaciones de momento con [BB::BBsolve()].
#' 3. Imputación-MNAR: modelo de resultado en respondientes + inclinación
#'    por gamma resuelta con [stats::uniroot()].
#' 4. DR-MNAR: combina propensión e imputación (peso sobre el residuo).
#'
#' @param z Brazo del instrumento aleatorizado (0/1).
#' @param r Indicador de respuesta a la pregunta (0/1).
#' @param y Resultado dicotómico; solo se usa cuando `r == 1`.
#' @param X Matriz de covariables con columna constante inicial.
#' @param w Pesos de diseño (NULL = iid). Se normalizan a media 1.
#' @param cluster,estrato Identificadores de UPM y estrato para la varianza
#'   sándwich agrupada (NULL = observaciones independientes).
#' @param gamma_inicial Valor inicial de gamma_y para BBsolve.
#' @param intervalo_gamma Intervalo de búsqueda de gamma para uniroot.
#' @return Lista con `pz`, `observado`, `ipw`, `imp`, `dr` (cada estimador
#'   con `y_est`, `se_y_est`, `gamma_y`, `se_gamma_y`), `n` y `convergencia`.
#' @keywords internal
#' @noRd
ajustar_nucleo_drmnar <- function(z, r, y, X, w = NULL,
                                  cluster = NULL, estrato = NULL,
                                  gamma_inicial = 1.1,
                                  intervalo_gamma = c(-5, 8)) {
  if (is.null(z)) {
    stop(
      "Se requiere el instrumento aleatorizado `z` (brazo control/",
      "tratamiento): sin instrumento gamma_y no está identificado."
    )
  }
  z <- as.numeric(z)
  r <- as.numeric(r)
  y <- as.numeric(y)
  if (length(unique(z[!is.na(z)])) < 2) {
    stop(
      "El instrumento `z` no tiene variación (todos en el mismo brazo): ",
      "gamma_y no está identificado."
    )
  }
  if (!all(y[r == 1] %in% c(0, 1))) {
    stop("`y` debe ser dicotómica (0/1) para los respondientes (r = 1).")
  }

  n <- length(z)
  if (is.null(w)) {
    w <- rep(1, n)
  }
  w <- w * n / sum(w) # normalizar a media 1 (estabilidad numérica)
  # el valor de y de los no respondientes no entra en ninguna ecuación;
  # se fija a 0 para evitar que codificaciones tipo 99 contaminen ZXY
  y <- ifelse(r == 1, y, 0)

  X <- as.matrix(X)
  k_x <- ncol(X)
  ZX <- cbind(X[, 1], z, X[, -1, drop = FALSE])
  ZXY <- cbind(ZX, y)
  ZX_int <- if (k_x > 1) {
    cbind(ZX, z * X[, -1, drop = FALSE])
  } else {
    ZX
  }

  # ---- (1) modelo del instrumento P(Z = 1 | X) ----
  pz <- ajustar_logit_ponderado(X, z, w)
  beta_pz <- pz$coefficients
  pz_fit <- as.numeric(expit(X %*% beta_pz))

  # ---- media observada (referencia "Observed" de Bailey) ----
  y_obs <- sum(w * r * y) / sum(w * r)
  n_eff_obs <- sum(w * r)^2 / sum(w^2 * r)
  se_obs <- sqrt(y_obs * (1 - y_obs) / n_eff_obs)

  # ---- (2) IPW-MNAR ----
  resp0 <- ajustar_logit_ponderado(ZX, r, w)
  par0 <- c(resp0$coefficients, gamma_inicial)
  ans_ipw <- BB::BBsolve(
    par = par0,
    fn = function(p) ec_ipw(p, r = r, ZXY = ZXY, pz_fit = pz_fit, w = w),
    quiet = TRUE,
    control = list(noimp = 240, maxit = 2000)
  )
  # la convergencia se juzga por el residuo de las ecuaciones de momento
  # (por observación), no por el código interno de BBsolve: con pesos de
  # diseño dispersos BBsolve encuentra la raíz (residuo ~1e-6) pero puede
  # reportar códigos != 0 por su criterio de norma escalada
  residuo_ipw <- max(abs(
    ec_ipw(ans_ipw$par, r = r, ZXY = ZXY, pz_fit = pz_fit, w = w)
  ))
  conv_ipw <- ans_ipw$convergence == 0 || (residuo_ipw / n) < 1e-6
  par_ipw <- unname(ans_ipw$par)
  gamma_ipw <- par_ipw[length(par_ipw)]
  pr_r_ipw <- as.numeric(expit(ZXY %*% par_ipw))
  y_ipw <- sum(w * r * y / pr_r_ipw) / sum(w)

  theta_ipw <- c(beta_pz, par_ipw, y_ipw)
  se_ipw <- calcular_sandwich(
    function(th) momentos_ipw(th, z, r, y, X, ZX, ZXY, w),
    theta = theta_ipw, cluster = cluster, estrato = estrato
  )
  se_y_ipw <- se_ipw[length(se_ipw)]
  se_g_ipw <- se_ipw[length(se_ipw) - 1]

  # ---- (3) Imputación-MNAR ----
  obs <- r == 1
  pcc <- ajustar_logit_ponderado(ZX_int[obs, , drop = FALSE], y[obs], w[obs])
  delta <- pcc$coefficients
  pr_y <- as.numeric(expit(ZX_int %*% delta))

  gamma_imp <- tryCatch(
    stats::uniroot(
      function(g) ec_imp(g, z, r, y, pr_y, pz_fit, w),
      interval = intervalo_gamma, extendInt = "yes", maxiter = 2000
    )$root,
    error = function(e) NA_real_
  )
  if (is.na(gamma_imp)) {
    warning("uniroot no encontró raíz para gamma (imputación).")
    y_imp <- NA_real_
    se_y_imp <- NA_real_
    se_g_imp <- NA_real_
  } else {
    y_imp <- sum(w * (r * y + (1 - r) * p_no_obs(gamma_imp, 1, pr_y))) / sum(w)
    theta_imp <- c(beta_pz, delta, gamma_imp, y_imp)
    se_imp <- calcular_sandwich(
      function(th) momentos_imp(th, z, r, y, X, ZX_int, w),
      theta = theta_imp, cluster = cluster, estrato = estrato
    )
    se_y_imp <- se_imp[length(se_imp)]
    se_g_imp <- se_imp[length(se_imp) - 1]
  }

  # ---- (4) DR-MNAR ----
  alpha_ipw <- par_ipw[seq_len(ncol(ZX))]
  dr_arg <- as.numeric(ZX %*% alpha_ipw)
  gamma_dr <- tryCatch(
    stats::uniroot(
      function(g) ec_dr(g, z, r, y, pr_y, pz_fit, dr_arg, w),
      interval = intervalo_gamma, extendInt = "yes", maxiter = 2000
    )$root,
    error = function(e) NA_real_
  )
  if (is.na(gamma_dr)) {
    warning("uniroot no encontró raíz para gamma (doblemente robusto).")
    y_dr <- NA_real_
    se_y_dr <- NA_real_
    se_g_dr <- NA_real_
  } else {
    p0_dr <- p_no_obs(gamma_dr, 1, pr_y)
    y_dr <- sum(
      w * (r / expit(dr_arg + gamma_dr * y) * (y - p0_dr) + p0_dr)
    ) / sum(w)
    theta_dr <- c(beta_pz, alpha_ipw, gamma_dr, delta, y_dr)
    se_dr <- calcular_sandwich(
      function(th) momentos_dr(th, z, r, y, X, ZX, ZXY, ZX_int, w),
      theta = theta_dr, cluster = cluster, estrato = estrato
    )
    se_y_dr <- se_dr[length(se_dr)]
    se_g_dr <- se_dr[k_x + ncol(ZX) + 1]
  }

  list(
    pz = list(coeficientes = beta_pz, ajustados = pz_fit),
    observado = list(y_est = y_obs, se_y_est = se_obs),
    ipw = list(
      y_est = y_ipw, se_y_est = se_y_ipw,
      gamma_y = gamma_ipw, se_gamma_y = se_g_ipw,
      pesos_mnar = 1 / pr_r_ipw, coeficientes = par_ipw
    ),
    imp = list(
      y_est = y_imp, se_y_est = se_y_imp,
      gamma_y = gamma_imp, se_gamma_y = se_g_imp,
      pr_y = pr_y, coeficientes = delta
    ),
    dr = list(
      y_est = y_dr, se_y_est = se_y_dr,
      gamma_y = gamma_dr, se_gamma_y = se_g_dr
    ),
    n = n,
    convergencia = conv_ipw && !is.na(gamma_imp) && !is.na(gamma_dr)
  )
}
