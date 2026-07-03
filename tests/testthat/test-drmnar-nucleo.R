# Tests del núcleo DR-MNAR: primitivas de las ecuaciones de estimación
# (port de Bailey/Sun et al.) generalizadas a pesos de diseño, y errores
# estándar sándwich con clusterización por UPM/estrato.
#
# Las funciones probadas son internas de encuestar (no exportadas):
#   expit(), p_no_obs(), ec_ipw(), ec_imp(), ec_dr(),
#   calcular_sandwich(), ajustar_nucleo_drmnar()

test_that("expit es la logística inversa", {
  expect_equal(expit(0), 0.5)
  expect_equal(expit(1.5), 1 / (1 + exp(-1.5)))
  expect_equal(expit(c(-Inf, Inf)), c(0, 1))
})

test_that("p_no_obs inclina P(Y|R=0) por el parámetro de selección gamma", {
  pr_y <- c(0.2, 0.5, 0.8)
  # gamma = 0: sin sesgo de selección, la probabilidad no se inclina
  expect_equal(p_no_obs(0, 1, pr_y), pr_y)
  expect_equal(p_no_obs(0, 0, pr_y), 1 - pr_y)
  # es una densidad: suma 1 sobre y in {0, 1}
  expect_equal(p_no_obs(2, 1, pr_y) + p_no_obs(2, 0, pr_y), rep(1, 3))
  # forma analítica de la inclinación odds-ratio (Sección 4 del anexo)
  g <- 1.3
  expect_equal(p_no_obs(g, 1, 0.5), exp(-g) * 0.5 / (exp(-g) * 0.5 + 0.5))
  # gamma > 0 (los Y=1 responden más) => entre los NO respondientes hay
  # menos Y=1 que entre los respondientes
  expect_true(all(p_no_obs(2, 1, pr_y) < pr_y))
})

test_that("ec_ipw se anula (aprox.) en los parámetros verdaderos", {
  sim <- simular_drmnar(n = 40000, gamma_y = 1.5, semilla = 99)
  d <- sim$d
  # ZXY con el y verdadero (aquí conocido); en los datos reales y solo se
  # usa cuando r = 1 y las ecuaciones no dependen de y para r = 0
  ZXY <- cbind(1, d$z, d$x, ifelse(d$r == 1, sim$y_verdadera, 0))
  par_verdadero <- c(-0.25, 1, 0.5, 1.5) # g_0, g_z, g_x, gamma_y
  pz_fit <- stats::plogis(0 + 0.4 * d$x)
  h <- ec_ipw(par_verdadero, r = d$r, ZXY = ZXY, pz_fit = pz_fit,
              w = rep(1, nrow(d)))
  expect_length(h, 4)
  # momentos promedio ~ 0 en muestra grande
  expect_true(all(abs(h / nrow(d)) < 0.02))
})

test_that("las ecuaciones de momento son lineales en los pesos", {
  sim <- simular_drmnar(n = 500, gamma_y = 1, semilla = 3)
  d <- sim$d
  ZXY <- cbind(1, d$z, d$x, d$y)
  pz_fit <- rep(0.5, nrow(d))
  par <- c(-0.2, 0.9, 0.4, 1.2)
  w1 <- rep(1, nrow(d))
  h1 <- ec_ipw(par, d$r, ZXY, pz_fit, w = w1)
  h2 <- ec_ipw(par, d$r, ZXY, pz_fit, w = 2 * w1)
  expect_equal(h2, 2 * h1)
})

test_that("un peso w=2 equivale a duplicar la observación", {
  sim <- simular_drmnar(n = 300, gamma_y = 0.8, semilla = 11)
  d <- sim$d
  ZXY <- cbind(1, d$z, d$x, d$y)
  pz_fit <- rep(0.5, nrow(d))
  par <- c(-0.2, 0.9, 0.4, 1.2)

  w <- rep(1, nrow(d))
  w[1] <- 2
  h_pesado <- ec_ipw(par, d$r, ZXY, pz_fit, w = w)

  # duplicar la primera fila con peso 1
  d2 <- rbind(d[1, ], d)
  ZXY2 <- cbind(1, d2$z, d2$x, d2$y)
  pz2 <- rep(0.5, nrow(d2))
  h_duplicado <- ec_ipw(par, d2$r, ZXY2, pz2, w = rep(1, nrow(d2)))

  expect_equal(h_pesado, h_duplicado)
})

test_that("calcular_sandwich reproduce el EE analítico de una media iid", {
  set.seed(42)
  y <- rnorm(400, mean = 3, sd = 2)
  mu <- mean(y)
  # momento de una media: m_i = w_i * (y_i - mu)
  momentos_media <- function(theta) matrix(y - theta, nrow = 1)
  se <- calcular_sandwich(momentos_media, theta = mu)
  se_analitico <- sqrt(mean((y - mu)^2) / length(y))
  expect_equal(as.numeric(se), se_analitico, tolerance = 1e-8)
})

test_that("la clusterización duplica la varianza con clusters de pares idénticos", {
  set.seed(7)
  y_g <- rnorm(200)
  y <- rep(y_g, each = 2) # cada cluster: 2 observaciones idénticas
  cluster <- rep(seq_along(y_g), each = 2)
  mu <- mean(y)
  momentos_media <- function(theta) matrix(y - theta, nrow = 1)

  se_iid <- calcular_sandwich(momentos_media, theta = mu)
  se_cl <- calcular_sandwich(momentos_media, theta = mu, cluster = cluster)

  # Var clusterizada (CR0) = 2 x Var iid cuando las obs dentro del cluster
  # son copias perfectas
  expect_equal(as.numeric(se_cl^2), as.numeric(2 * se_iid^2), tolerance = 1e-8)
})

test_that("ajustar_nucleo_drmnar recupera gamma~0 y la media bajo MCAR", {
  sim <- simular_drmnar(n = 5000, gamma_y = 0, g_z = 1, semilla = 2026)
  d <- sim$d
  res <- ajustar_nucleo_drmnar(
    z = d$z, r = d$r, y = d$y, X = sim$X, w = NULL
  )
  # estructura del resultado
  expect_named(
    res,
    c("pz", "observado", "ipw", "imp", "dr", "n", "convergencia"),
    ignore.order = TRUE
  )
  for (comp in c("ipw", "imp", "dr")) {
    expect_true(all(c("y_est", "se_y_est", "gamma_y", "se_gamma_y") %in%
                      names(res[[comp]])))
  }
  # bajo MCAR el parámetro de no ignorabilidad es ~0 y el IC lo cubre
  expect_lt(abs(res$dr$gamma_y), 0.45)
  expect_true(abs(res$dr$gamma_y) < 1.96 * res$dr$se_gamma_y + 1e-9)
  # y la media estimada DR está cerca de la verdadera
  expect_lt(abs(res$dr$y_est - sim$media_verdadera), 0.03)
})

test_that("ajustar_nucleo_drmnar detecta MNAR y corrige la media (gamma=2)", {
  sim <- simular_drmnar(n = 8000, gamma_y = 2, g_z = 1.5, semilla = 4)
  d <- sim$d
  res <- ajustar_nucleo_drmnar(z = d$z, r = d$r, y = d$y, X = sim$X)

  media_observada <- mean(d$y[d$r == 1])
  # la media observada está sesgada hacia arriba (los Y=1 responden más)
  expect_gt(media_observada, sim$media_verdadera + 0.05)
  # el DR corrige: queda mucho más cerca de la media verdadera
  expect_lt(abs(res$dr$y_est - sim$media_verdadera),
            abs(media_observada - sim$media_verdadera) / 2)
  # gamma estimado cerca de 2 y significativo
  expect_lt(abs(res$dr$gamma_y - 2), 0.6)
  expect_gt(res$dr$gamma_y - 1.96 * res$dr$se_gamma_y, 0)
})

test_that("ajustar_nucleo_drmnar exige el instrumento z con variación", {
  sim <- simular_drmnar(n = 200, gamma_y = 0, semilla = 8)
  d <- sim$d
  expect_error(
    ajustar_nucleo_drmnar(z = rep(1, nrow(d)), r = d$r, y = d$y, X = sim$X),
    regexp = "instrumento"
  )
  expect_error(
    ajustar_nucleo_drmnar(z = NULL, r = d$r, y = d$y, X = sim$X),
    regexp = "instrumento"
  )
})

test_that("los pesos de diseño mueven la estimación hacia la media ponderada", {
  # dos estratos con medias distintas; el peso sobre-representa al estrato 2.
  # n grande porque gamma se estima con mucha varianza y su error se
  # amplifica en la media DR (probado empíricamente: a n = 6000 el ruido
  # de gamma domina la comparación).
  sim <- simular_drmnar(n = 30000, gamma_y = 0, g_z = 1.5, semilla = 12)
  d <- sim$d
  w <- ifelse(d$x == 1, 3, 1) # x define el "estrato"
  res_w <- ajustar_nucleo_drmnar(z = d$z, r = d$r, y = d$y, X = sim$X, w = w)
  res_1 <- ajustar_nucleo_drmnar(z = d$z, r = d$r, y = d$y, X = sim$X)

  media_pond <- stats::weighted.mean(sim$y_verdadera, w)
  media_simple <- mean(sim$y_verdadera)
  # la versión ponderada apunta a la media ponderada, no a la simple
  expect_lt(abs(res_w$dr$y_est - media_pond), abs(res_w$dr$y_est - media_simple))
  expect_lt(abs(res_1$dr$y_est - media_simple), abs(res_1$dr$y_est - media_pond))
  expect_lt(abs(res_w$dr$y_est - media_pond), 0.03)
})
