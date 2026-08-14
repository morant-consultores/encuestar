# Armado del deck de diagnóstico DR-MNAR (pptx) desde el paquete.

#' Deck de diagnóstico de no respuesta no ignorable (pptx)
#'
#' Arma el deck completo del diagnóstico DR-MNAR desde el paquete: las láminas
#' que van del diseño y su validez al dictamen de ponderación, con las lecturas
#' de `gamma` en lenguaje de negocio.
#'
#' Antes de existir esta función cada estudio portaba su propio script de ~230
#' líneas; ya habían divergido entre sí y dos láminas se armaban con `officer`
#' inline, fuera del contrato de que todo el graficado sale del paquete. Lo que
#' cambia entre estudios entra por parámetro.
#'
#' El orden de las láminas cuenta el argumento completo: diseño y validez ->
#' diagnóstico e impacto -> profundización y costo -> validación robusta ->
#' dictamen. La validez del instrumento va ANTES del diagnóstico porque si no se
#' sostiene, no hay nada que leer después.
#'
#' Toda lámina cuyo insumo salga vacío se OMITE en vez de reventar, para que el
#' deck sirva a estudios en distintas etapas de campo.
#'
#' @param diseno Objeto `survey::svydesign` del estudio.
#' @param bundle Objeto `diseno_drmnar` de [generar_diseno_drmnar()].
#' @param salida Ruta del `.pptx` a escribir.
#' @param plantilla Ruta al `.pptx` plantilla; `NULL` usa la default de officer.
#' @param titulo,subtitulo Textos de la portada.
#' @param preguntas_clave Preguntas headline que se grafican como REFERENCIA
#'   aunque su no respuesta sea ignorable (control de que los siete estimadores
#'   coincidan cuando gamma ~ 0).
#' @param subgrupos Lista nombrada de vectores lógicos para la lámina de
#'   heterogeneidad. `NULL` (default) omite la lámina; encenderla recalcula el
#'   diagnóstico por subgrupo, que es caro.
#' @param catalogo_covariables `data.frame` con `covariable`, `tipo`, `fuente` y
#'   `mide`; `NULL` usa el `covariables_doc` del bundle.
#' @param nota Nota al pie de cada lámina.
#' @param top_gammas Máximo de preguntas en el caterpillar.
#' @param metodo_multiplicidad Método de [ajustar_multiplicidad_norespuesta()].
#' @param layout,master Layout y master de la plantilla.
#' @return La ruta de `salida`, invisible.
#' @export
armar_deck_norespuesta <- function(diseno, bundle, salida,
                                   plantilla = NULL,
                                   titulo = "Diagnóstico de no respuesta (DR-MNAR)",
                                   subtitulo = "",
                                   preguntas_clave = NULL,
                                   subgrupos = NULL,
                                   catalogo_covariables = NULL,
                                   nota = NULL,
                                   top_gammas = 30,
                                   metodo_multiplicidad = "BH",
                                   layout = "una_graf",
                                   master = "Tema de Office") {
  bd <- diseno$variables
  diag <- bundle$diagnostico
  covs <- bundle$covariables %||% character(0)
  instrumento <- bundle$instrumento %||% "drmnar_z"
  n_ef <- nrow(bd)

  pptx <- if (is.null(plantilla)) {
    officer::read_pptx()
  } else {
    officer::read_pptx(plantilla)
  }

  # portada
  pptx <- officer::add_slide(pptx, layout = layout, master = master)
  pptx <- .ph_seguro(pptx, titulo, "titulo")
  pptx <- .ph_seguro(pptx, subtitulo, "subtitulo")

  agregar <- function(pptx, encabezado, plot) {
    if (is.null(plot)) return(pptx)
    pptx <- officer::add_slide(pptx, layout = layout, master = master)
    pptx <- .ph_seguro(pptx, encabezado, "titulo_gral")
    pptx <- .ph_seguro(pptx, plot, "objeto_1")
    if (!is.null(nota)) pptx <- .ph_seguro(pptx, nota, "num_entrevistas")
    pptx
  }

  # flujo del instrumento
  pptx <- agregar(
    pptx, "Flujo del instrumento aleatorizado (protocolo de campo)",
    .try_null(graficar_flujo_norespuesta(resumen_flujo_norespuesta(bd))))

  # validez del instrumento, antes del diagnóstico
  pptx <- agregar(
    pptx, "Balance del instrumento — supuesto de exclusión",
    .try_null(graficar_balance_instrumento(
      evaluar_instrumento_norespuesta(bd, covs))))

  # covariables del ajuste
  doc <- catalogo_covariables %||% bundle$covariables_doc
  if (!is.null(doc) && nrow(doc) > 0) {
    pptx <- agregar(
      pptx, "Covariables del ajuste doblemente robusto",
      .try_null(graficar_tabla_covariables(
        doc, n_ef, bundle$covariables_umbral %||% 1200L,
        bundle$covariables_ricas %||% covs)))
  }

  # caterpillar de gamma, solo las DR-MNAR
  dr_diag <- diag |>
    dplyr::filter(.data$decision == "DR-MNAR", is.finite(.data$gamma_y)) |>
    dplyr::arrange(dplyr::desc(abs(.data$gamma_y))) |>
    utils::head(top_gammas)
  if (nrow(dr_diag) > 0) {
    pptx <- agregar(pptx, sprintf(
      "Gamma de no ignorabilidad — preguntas DR-MNAR (%d de %d)",
      nrow(dr_diag), nrow(diag)),
      .try_null(graficar_diagnostico_norespuesta(dr_diag)))
  }

  # impacto práctico
  pptx <- agregar(pptx, "Impacto práctico: Raking vs DR-MNAR",
                  .try_null(graficar_impacto_drmnar(diag)))

  # comparación de estimadores y precisión, por pregunta
  preguntas_dr <- bundle$decision$pregunta[bundle$decision$decision == "DR-MNAR"]
  clave <- intersect(preguntas_clave %||% character(0), diag$pregunta)
  for (cod in unique(c(clave, preguntas_dr))) {
    cat_ref <- diag$categoria[diag$pregunta == cod][1]
    est <- .try_null(estimar_drmnar(
      diseno, pregunta = cod, covariables = covs,
      instrumento = instrumento, categoria = cat_ref))
    if (is.null(est)) next
    etq <- if (cod %in% preguntas_dr) "DR-MNAR" else "referencia (ignorable)"
    pptx <- agregar(
      pptx, sprintf("Comparación de estimadores — %s = \"%s\"  [%s]",
                    cod, cat_ref, etq),
      .try_null(graficar_comparacion_estimadores(est)))
    if (cod %in% preguntas_dr) {
      pptx <- agregar(pptx, sprintf("Precisión (EE sándwich) — %s", cod),
                      .try_null(graficar_precision_drmnar(est)))
    }
  }

  # pesos de propensión inversa
  if (length(preguntas_dr) > 0) {
    cat_pesos <- diag$categoria[diag$pregunta == preguntas_dr[1]][1]
    pptx <- agregar(
      pptx, paste0("Pesos de propensión inversa — ", preguntas_dr[1]),
      .try_null(graficar_pesos_drmnar(stats::weights(
        diseno_para_pregunta(bundle, preguntas_dr[1],
                             categoria = cat_pesos)))))
  }

  # validación con desertores
  es_dr <- diag$decision == "DR-MNAR"
  if (any(es_dr)) {
    preguntas_lista <- stats::setNames(
      as.list(diag$categoria[es_dr]), diag$pregunta[es_dr])
    pptx <- agregar(
      pptx, "Validación con los desertores del filtro temático",
      .try_null(graficar_desertores_norespuesta(
        comparar_desertores_norespuesta(bd, preguntas_lista), diag)))
  }

  # multiplicidad
  pptx <- agregar(
    pptx, "¿Cuáles detecciones sobreviven al azar?",
    .try_null(graficar_multiplicidad_norespuesta(
      ajustar_multiplicidad_norespuesta(diag, metodo = metodo_multiplicidad))))

  # dictamen
  pptx <- agregar(pptx, "Decisión por pregunta: DR-MNAR vs Raking",
                  .try_null(graficar_decision_norespuesta(diag)))

  # opcional: heterogeneidad por subgrupo
  if (!is.null(subgrupos)) {
    diag_sub <- .try_null(diagnosticar_norespuesta(
      diseno = diseno,
      preguntas = stats::setNames(as.list(diag$categoria), diag$pregunta),
      covariables = covs, instrumento = instrumento,
      subconjuntos = subgrupos))
    pptx <- agregar(pptx, "Heterogeneidad de gamma por subgrupo",
                    .try_null(graficar_heterogeneidad_norespuesta(diag_sub)))
  }

  dir.create(dirname(salida), showWarnings = FALSE, recursive = TRUE)
  print(pptx, target = salida)
  invisible(salida)
}

# Coloca un elemento en su placeholder; si la plantilla no lo tiene, cae al
# cuerpo y, si tampoco, deja la lámina como está en vez de tumbar el deck.
.ph_seguro <- function(pptx, valor, etiqueta) {
  if (is.null(valor)) return(pptx)
  tryCatch(
    officer::ph_with(pptx, value = valor,
                     location = officer::ph_location_label(etiqueta)),
    error = function(e) tryCatch(
      officer::ph_with(pptx, value = valor,
                       location = officer::ph_location_type(type = "body")),
      error = function(e2) pptx)
  )
}

# Una lámina que no se puede calcular se omite; el deck sigue.
.try_null <- function(expr) tryCatch(expr, error = function(e) NULL)
