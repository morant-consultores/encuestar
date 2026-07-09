# Helpers para los tests del módulo DR-MNAR (estimación doblemente robusta
# bajo no respuesta no ignorable, Bailey 2023 / Sun et al. 2018).

# Directorio con la réplica oficial de Bailey (datos IPSOS + resultados
# publicados). Se resuelve por variable de entorno o por la ruta relativa
# del repositorio hermano "doubly robust estimator". Si no existe, los
# tests de replicación se saltan (p. ej. en CI).
dir_replicacion_bailey <- function() {
  dir <- Sys.getenv("DRMNAR_REPLICATION_DIR", unset = "")
  if (nzchar(dir)) {
    return(dir)
  }
  file.path(
    testthat::test_path(),
    "..", "..", "..",
    "doubly robust estimator", "Replication"
  )
}

# Prepara los datos de IPSOS Feb-2019 exactamente como dr_FUNCTION() de
# Bailey para la especificación Turnout / All (Figura 3), incluyendo la
# semilla 7 con la que Bailey asigna brazo a los no respondientes.
cargar_datos_bailey_turnout <- function(dir_replicacion) {
  dta <- haven::read_dta(file.path(dir_replicacion, "data", "IPSOS_Feb2019.dta"))

  dta$Choice.all <- dta$Choice
  set.seed(7)
  dta$Choice.all[dta$SurveyNonRespondent == 1] <-
    stats::rbinom(sum(dta$SurveyNonRespondent), 1, prob = 0.5)

  # turnoutCutpoint = 5 en Bailey: y = 1*(Turnout > 4); NA -> 99 (irrelevante
  # porque y solo entra a las ecuaciones multiplicada por r).
  y <- 1L * (dta$Turnout > 4)
  y[is.na(y)] <- 99L

  data.frame(
    z = as.numeric(dta$Choice.all),
    r = as.numeric(dta$Turnout_HeckObsDum),
    y = as.numeric(y),
    Female = as.numeric(dta$Female),
    Black = as.numeric(dta$Black),
    Hispanic = as.numeric(dta$Hispanic),
    SomeCollege = as.numeric(dta$SomeCollege),
    College = as.numeric(dta$College),
    Grad = as.numeric(dta$Grad),
    Age = as.numeric(dta$PPAGE)
  )
}

# Resultados publicados por Bailey para Turnout / All
# (Replication/results/estimation_results_Turnout_All.csv).
resultados_bailey_turnout <- function() {
  list(
    observado = list(y_est = 0.7775, se_y_est = 0.0107),
    ipw = list(y_est = 0.5512, se_y_est = 0.0329, gamma_y = 2.0239, se_gamma_y = 0.3641),
    imp = list(y_est = 0.5636, se_y_est = 0.0156, gamma_y = 1.9296, se_gamma_y = 0.4149),
    dr = list(y_est = 0.5676, se_y_est = 0.0391, gamma_y = 1.9095, se_gamma_y = 0.4293)
  )
}

# Construye un survey::svydesign sintético con el contrato de columnas del
# snapshot DR-MNAR: instrumento `drmnar_z`, respuesta por pregunta y la
# pregunta categórica ("Sí"/"No", NA para quien no respondió el ítem).
crear_diseno_sintetico <- function(n = 8000, gamma_y = 0, g_z = 1.5,
                                   semilla = 20260702, w = NULL,
                                   n_clusters = NULL, ...) {
  sim <- simular_drmnar(n = n, gamma_y = gamma_y, g_z = g_z,
                        semilla = semilla, ...)
  datos <- sim$d
  datos$drmnar_z <- datos$z
  datos$drmnar_r <- datos$r
  datos$drmnar_tema <- ifelse(
    datos$z == 0,
    NA_character_,
    ifelse(datos$r == 1, "Política", "Deportes")
  )
  datos$conoce_cand <- ifelse(
    datos$r == 1,
    ifelse(sim$y_verdadera == 1, "Sí lo conoce", "No lo conoce"),
    NA_character_
  )
  datos$peso <- if (is.null(w)) rep(1, n) else w
  datos$upm <- if (is.null(n_clusters)) {
    seq_len(n)
  } else {
    rep_len(seq_len(n_clusters), n)
  }
  diseno <- survey::svydesign(
    ids = ~upm, weights = ~peso, data = datos
  )
  list(
    diseno = diseno,
    media_verdadera = sim$media_verdadera,
    y_verdadera = sim$y_verdadera,
    datos = datos
  )
}

# Generador de datos sintéticos con mecanismo de respuesta conocido.
#   P(Z=1|X) = expit(a_z + b_z * x)
#   P(Y=1|X) = expit(a_y + b_y * x)
#   P(R=1|Z,X,Y) = expit(g_0 + g_z * z + g_x * x + gamma_y * y)
# gamma_y = 0 => MAR; gamma_y != 0 => MNAR.
simular_drmnar <- function(n, gamma_y, g_z = 1, g_0 = -0.25, g_x = 0.5,
                           a_z = 0, b_z = 0.4, a_y = -0.2, b_y = 0.8,
                           semilla = 20260702) {
  set.seed(semilla)
  x <- stats::rbinom(n, 1, 0.5)
  z <- stats::rbinom(n, 1, stats::plogis(a_z + b_z * x))
  y <- stats::rbinom(n, 1, stats::plogis(a_y + b_y * x))
  pr_r <- stats::plogis(g_0 + g_z * z + g_x * x + gamma_y * y)
  r <- stats::rbinom(n, 1, pr_r)
  list(
    d = data.frame(z = z, r = r, y = ifelse(r == 1, y, 0), x = x),
    y_verdadera = y,
    media_verdadera = mean(y),
    X = cbind(1, x)
  )
}
