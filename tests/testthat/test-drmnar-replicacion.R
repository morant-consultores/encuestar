# Replicación de Bailey (2023) "Countering Non-Ignorable Nonresponse..."
# especificación Turnout / All (Figura 3 del paper).
#
# Con pesos = 1 (iid) el núcleo DR-MNAR de encuestar debe reproducir los
# resultados publicados en Replication/results/estimation_results_Turnout_All.csv.
# Este test ancla la fidelidad del port ANTES de generalizar a pesos de
# diseño y clusterización.
#
# Nota sobre tolerancias del EE del DR: mm.dr_gen() en la réplica original
# de Bailey indexa mal los coeficientes del modelo de imputación al armar
# el "meat" del sándwich (usa g[(k_x+1):...] en lugar de los índices
# posteriores al bloque ZXY que sí usa M.dr_gen()). Nuestro port usa los
# índices correctos, por lo que el EE del DR puede diferir moderadamente
# del publicado; los estimadores puntuales y gamma no se ven afectados.

test_that("el nucleo reproduce Turnout/All de Bailey con pesos unitarios", {
  skip_if_not_installed("haven")
  skip_if_not_installed("BB")
  dir_rep <- dir_replicacion_bailey()
  skip_if_not(
    file.exists(file.path(dir_rep, "data", "IPSOS_Feb2019.dta")),
    "Réplica de Bailey no disponible (define DRMNAR_REPLICATION_DIR)"
  )

  d <- cargar_datos_bailey_turnout(dir_rep)
  esperado <- resultados_bailey_turnout()

  X <- cbind(1, d$Female, d$Black, d$Hispanic, d$SomeCollege,
             d$College, d$Grad, d$Age)

  res <- ajustar_nucleo_drmnar(
    z = d$z, r = d$r, y = d$y, X = X,
    gamma_inicial = 1.1
  )

  # --- media observada (fila "Observed" de Bailey) ---
  expect_equal(res$observado$y_est, esperado$observado$y_est, tolerance = 1e-3)
  expect_equal(res$observado$se_y_est, esperado$observado$se_y_est,
               tolerance = 0.05)

  # --- IPW-NINR ---
  expect_equal(res$ipw$y_est, esperado$ipw$y_est, tolerance = 2e-3)
  expect_equal(res$ipw$gamma_y, esperado$ipw$gamma_y, tolerance = 2e-2)
  expect_equal(res$ipw$se_y_est, esperado$ipw$se_y_est, tolerance = 0.10)
  expect_equal(res$ipw$se_gamma_y, esperado$ipw$se_gamma_y, tolerance = 0.10)

  # --- Imputación-NINR ---
  expect_equal(res$imp$y_est, esperado$imp$y_est, tolerance = 2e-3)
  expect_equal(res$imp$gamma_y, esperado$imp$gamma_y, tolerance = 2e-2)
  expect_equal(res$imp$se_y_est, esperado$imp$se_y_est, tolerance = 0.10)
  expect_equal(res$imp$se_gamma_y, esperado$imp$se_gamma_y, tolerance = 0.10)

  # --- Doblemente robusto-NINR ---
  expect_equal(res$dr$y_est, esperado$dr$y_est, tolerance = 2e-3)
  expect_equal(res$dr$gamma_y, esperado$dr$gamma_y, tolerance = 2e-2)
  # EE del DR: tolerancia amplia por el bug de indexación de mm.dr_gen en
  # la réplica original (ver nota al inicio del archivo)
  expect_equal(res$dr$se_y_est, esperado$dr$se_y_est, tolerance = 0.35)
  expect_equal(res$dr$se_gamma_y, esperado$dr$se_gamma_y, tolerance = 0.35)

  expect_true(res$convergencia)
})
