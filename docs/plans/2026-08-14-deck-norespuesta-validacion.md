# Deck DR-MNAR reutilizable — Plan de implementación

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Que el deck de diagnóstico DR-MNAR se arme desde `encuestar` con seis
láminas nuevas de validación e impacto, y que cualquier encuesta lo obtenga con
~20 líneas de configuración en vez de portar un script de 230.

**Architecture:** Once funciones de contenido exportadas (cálculo + gráfica, cada
una con una responsabilidad) más un armador `armar_deck_norespuesta()` que las
orquesta contra una plantilla `officer`. Ninguna función inventa cálculo nuevo
donde el paquete ya lo produce: el impacto sale de `est_rake`/`est_drmnar` del
diagnóstico y la precisión del error estándar sándwich de `estimar_drmnar`.

**Tech Stack:** R, ggplot2, officer, survey, dplyr, testthat (edition 3), roxygen2.

**Spec:** `docs/specs/2026-08-14-deck-norespuesta-validacion-design.md`

## Global Constraints

- Repo: `encuestar`, rama `feat/norespuesta-validacion-bailey` (ya creada, PR #302 borrador).
- Toda función pública lleva roxygen con `@param`, `@return` y `@export`, y
  `devtools::document()` corre antes de cada commit.
- Colores fijos del módulo, ya definidos en `R/graficar_norespuesta.R`:
  `COLOR_MORANT <- "#A6032F"`, `COLOR_NEUTRO <- "#4C5B61"`.
- Tema gráfico: `tema_morant()`.
- Los tests usan los helpers que YA existen en `tests/testthat/helper-drmnar.R`:
  `crear_diseno_sintetico(n, gamma_y, semilla, ...)` y `simular_drmnar(...)`.
- Columnas de `diagnosticar_norespuesta()`: `pregunta`, `categoria`,
  `subconjunto`, `gamma_y`, `ee`, `inf`, `sup`, `z_stat`, `no_ignorable`,
  `decision`, `n`, `convergencia`, `est_drmnar`, `est_rake`, `diferencia`.
- Columnas de `estimar_drmnar()`: `modelo`, `est`, `ee`, `inf`, `sup`, `gamma_y`,
  `ee_gamma_y`, `pregunta`, `categoria`, `subconjunto`, `n`, `convergencia`.
- Columnas del snapshot: `drmnar_z` (0/1), `drmnar_r` (0/1), `drmnar_tema`.
- Ninguna función revienta con cero filas: devuelve `NULL` (gráficas) o un tibble
  vacío con las columnas declaradas (cálculo).
- Comandos: `Rscript -e 'devtools::load_all("."); testthat::test_file("<ruta>")'`
  desde la raíz de `encuestar`.

---

### Task 1: Partir `R/graficar_norespuesta.R`

**Files:**
- Modify: `R/graficar_norespuesta.R` (391 líneas → solo las 6 gráficas base + R6)
- Create: `R/norespuesta_lectura.R` (vacío con encabezado)
- Create: `R/norespuesta_validacion.R` (vacío con encabezado)
- Create: `R/graficar_norespuesta_validacion.R` (vacío con encabezado)
- Create: `R/deck_norespuesta.R` (vacío con encabezado)

**Interfaces:**
- Consumes: nada.
- Produces: los cuatro archivos donde caen las tareas 2–9. Ninguna función
  cambia de nombre ni de firma, así que `NAMESPACE` no se altera.

- [ ] **Step 1: Verificar el estado verde de partida**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-graficar-norespuesta.R")'`
Expected: PASS, 25 pruebas.

- [ ] **Step 2: Crear los cuatro archivos con su encabezado**

```r
# R/norespuesta_lectura.R
# Traducción de gamma a lenguaje de negocio para las láminas del deck.

# R/norespuesta_validacion.R
# Cálculo de las validaciones del diagnóstico DR-MNAR: control de
# multiplicidad, contraste con los desertores del filtro temático y balance
# de covariables por brazo del instrumento.

# R/graficar_norespuesta_validacion.R
# Gráficas de validación e impacto del diagnóstico DR-MNAR.

# R/deck_norespuesta.R
# Armado del deck de diagnóstico DR-MNAR (pptx) desde el paquete.
```

- [ ] **Step 3: Correr los tests otra vez**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS, sin cambios en el conteo.

- [ ] **Step 4: Commit**

```bash
git add R/norespuesta_lectura.R R/norespuesta_validacion.R \
        R/graficar_norespuesta_validacion.R R/deck_norespuesta.R
git commit -m "refactor(norespuesta): los archivos donde cae el deck de validación

R/graficar_norespuesta.R lleva 391 líneas y las seis láminas nuevas lo
duplicarían. Se abren los archivos por responsabilidad -cálculo, gráfica,
lectura y armado- antes de escribir nada, para que las funciones nazcan en su
lugar y no haya que moverlas después."
```

---

### Task 2: `lectura_norespuesta()`

**Files:**
- Modify: `R/norespuesta_lectura.R`
- Test: `tests/testthat/test-norespuesta-lectura.R`

**Interfaces:**
- Consumes: tibble de `diagnosticar_norespuesta()`.
- Produces: `lectura_norespuesta(diagnostico)` → tibble con `pregunta`,
  `categoria`, `subconjunto`, `estado` (`"sobre_representacion"` |
  `"sub_representacion"` | `"ignorable"`), `texto` (character). La usan la
  Task 9 (armador) y el estudio municipal.

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-norespuesta-lectura.R
fixture_lectura <- function() {
  tibble::tibble(
    pregunta = c("aprob_pm", "calif_horacio", "amai_autos", "amai_cuartos"),
    categoria = c("Aprueba mucho", "0", "1", NA_character_),
    subconjunto = "Estado",
    gamma_y = c(0.92, -3.12, 0.20, NA_real_),
    ee = c(0.30, 1.06, 0.30, NA_real_),
    inf = c(0.33, -5.19, -0.39, NA_real_),
    sup = c(1.52, -1.05, 0.79, NA_real_),
    z_stat = c(3.07, -2.95, 0.67, NA_real_),
    no_ignorable = c(TRUE, TRUE, FALSE, NA),
    decision = c("DR-MNAR", "DR-MNAR", "Raking", "Sin estimación"),
    convergencia = c(TRUE, TRUE, TRUE, FALSE),
    est_rake = c(0.65, 0.10, 0.30, NA_real_),
    est_drmnar = c(0.55, 0.18, 0.30, NA_real_)
  )
}

test_that("gamma positivo significativo se lee como sobre-representación", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "aprob_pm", ]
  expect_equal(fila$estado, "sobre_representacion")
  expect_match(fila$texto, "sobreestima")
  # el corrimiento en puntos porcentuales entra en el texto
  expect_match(fila$texto, "10")
})

test_that("gamma negativo significativo se lee como ocultamiento", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "calif_horacio", ]
  expect_equal(fila$estado, "sub_representacion")
  expect_match(fila$texto, "subestima")
})

test_that("IC que incluye 0 se lee como ignorable y recomienda raking", {
  lec <- lectura_norespuesta(fixture_lectura())
  fila <- lec[lec$pregunta == "amai_autos", ]
  expect_equal(fila$estado, "ignorable")
  expect_match(fila$texto, "raking|eficiente")
})

test_that("gamma no estimable o sin convergencia cae en ignorable", {
  lec <- lectura_norespuesta(fixture_lectura())
  expect_equal(lec$estado[lec$pregunta == "amai_cuartos"], "ignorable")
})

test_that("diagnóstico vacío devuelve tibble vacío con las columnas", {
  lec <- lectura_norespuesta(fixture_lectura()[0, ])
  expect_equal(nrow(lec), 0)
  expect_true(all(c("pregunta", "estado", "texto") %in% names(lec)))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-lectura.R")'`
Expected: FAIL, `could not find function "lectura_norespuesta"`.

- [ ] **Step 3: Implementar**

```r
#' Lectura en lenguaje de negocio del parámetro de no ignorabilidad
#'
#' Traduce cada fila del diagnóstico a una frase accionable según el signo de
#' `gamma_y` y su significancia. Tres estados excluyentes:
#'
#' * `sobre_representacion` (gamma > 0, IC excluye 0): quienes están en la
#'   categoría responden más, así que el raking SOBREESTIMA el indicador.
#' * `sub_representacion` (gamma < 0, IC excluye 0): quienes están en la
#'   categoría responden menos (ocultamiento), así que el raking SUBESTIMA.
#' * `ignorable` (IC incluye 0, gamma no estimable o sin convergencia): la no
#'   respuesta es ignorable y el raking es adecuado y más eficiente.
#'
#' El texto se arma desde los datos del corte, nunca desde una tabla fija, para
#' que no se desincronice cuando cambian los números.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()].
#' @return Tibble con `pregunta`, `categoria`, `subconjunto`, `estado` y `texto`.
#' @export
lectura_norespuesta <- function(diagnostico) {
  d <- diagnostico
  vacio <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    subconjunto = character(0), estado = character(0), texto = character(0)
  )
  if (nrow(d) == 0) return(vacio)

  conv <- if ("convergencia" %in% names(d)) !is.na(d$convergencia) & d$convergencia else TRUE
  estimable <- is.finite(d$gamma_y) & is.finite(d$inf) & is.finite(d$sup) & conv
  significativo <- estimable & (d$inf > 0 | d$sup < 0)

  estado <- ifelse(
    !significativo, "ignorable",
    ifelse(d$gamma_y > 0, "sobre_representacion", "sub_representacion")
  )

  etiqueta <- ifelse(
    is.na(d$categoria) | !nzchar(d$categoria),
    d$pregunta, paste0(d$pregunta, " = \"", d$categoria, "\"")
  )

  # el corrimiento solo se menciona si el diagnóstico lo trae calculado
  pp <- if (all(c("est_rake", "est_drmnar") %in% names(d))) {
    (d$est_drmnar - d$est_rake) * 100
  } else {
    rep(NA_real_, nrow(d))
  }
  frase_pp <- ifelse(
    is.finite(pp),
    sprintf(" El ajuste mueve la estimación %.1f puntos porcentuales.", pp),
    ""
  )

  texto <- dplyr::case_when(
    estado == "sobre_representacion" ~ sprintf(
      paste0("%s: quienes responden esta categoría participan MÁS en la encuesta ",
             "(gamma = %+.2f), así que el raking SOBREESTIMA el indicador.%s"),
      etiqueta, d$gamma_y, frase_pp),
    estado == "sub_representacion" ~ sprintf(
      paste0("%s: quienes responden esta categoría participan MENOS ",
             "(ocultamiento, gamma = %+.2f), así que el raking SUBESTIMA el ",
             "indicador.%s"),
      etiqueta, d$gamma_y, frase_pp),
    .default = sprintf(
      paste0("%s: no respuesta ignorable, el intervalo de gamma incluye el 0. ",
             "El raking es adecuado y más eficiente (menor varianza)."),
      etiqueta)
  )

  tibble::tibble(
    pregunta = d$pregunta, categoria = d$categoria,
    subconjunto = d$subconjunto, estado = estado, texto = texto
  )
}
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-lectura.R")'`
Expected: PASS, 5 pruebas.

- [ ] **Step 5: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/norespuesta_lectura.R tests/testthat/test-norespuesta-lectura.R \
        NAMESPACE man/lectura_norespuesta.Rd
git commit -m "feat(norespuesta): gamma se lee en lenguaje de negocio, no en signo

El deck mostraba el valor de gamma y dejaba la lectura al lector: un +0.92 no
dice por sí solo que el raking SOBREESTIMA la aprobación del alcalde porque los
entusiastas responden más. lectura_norespuesta() lo resuelve en tres estados
excluyentes -sobre-representación, ocultamiento e ignorable- y arma la frase
desde los datos del corte, para que no se desincronice cuando cambien."
```

---

### Task 3: `graficar_impacto_drmnar()`

**Files:**
- Modify: `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-validacion-graficas.R`

**Interfaces:**
- Consumes: tibble de `diagnosticar_norespuesta()` con `est_rake` y `est_drmnar`.
- Produces: `graficar_impacto_drmnar(diagnostico)` → `ggplot` o `NULL` si no hay
  filas DR-MNAR con estimaciones finitas.

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-norespuesta-validacion-graficas.R
fixture_impacto <- function() {
  tibble::tibble(
    pregunta = c("aprob_pm", "calif_horacio", "amai_autos"),
    categoria = c("Aprueba mucho", "0", "1"),
    subconjunto = "Estado",
    gamma_y = c(0.92, -3.12, 0.20),
    inf = c(0.33, -5.19, -0.39),
    sup = c(1.52, -1.05, 0.79),
    no_ignorable = c(TRUE, TRUE, FALSE),
    decision = c("DR-MNAR", "DR-MNAR", "Raking"),
    est_rake = c(0.65, 0.10, 0.30),
    est_drmnar = c(0.55, 0.18, 0.30),
    diferencia = c(-0.10, 0.08, 0.00)
  )
}

test_that("graficar_impacto_drmnar dibuja solo las preguntas DR-MNAR", {
  g <- graficar_impacto_drmnar(fixture_impacto())
  expect_s3_class(g, "ggplot")
  # 2 preguntas DR-MNAR x 2 estimadores = 4 puntos; amai_autos queda fuera
  expect_equal(nrow(g$data), 4)
  expect_false("amai_autos" %in% g$data$pregunta)
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms)
  expect_true("GeomPoint" %in% geoms)
})

test_that("graficar_impacto_drmnar devuelve NULL sin filas DR-MNAR", {
  solo_raking <- fixture_impacto()[3, ]
  expect_null(graficar_impacto_drmnar(solo_raking))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: FAIL, `could not find function "graficar_impacto_drmnar"`.

- [ ] **Step 3: Implementar**

```r
#' Impacto práctico de corregir por no respuesta no ignorable
#'
#' Dumbbell del corrimiento de la estimación al pasar del raking tradicional
#' (`Weights-MAR`) al doblemente robusto MNAR, para las preguntas cuya decisión
#' es DR-MNAR. Responde la pregunta que el caterpillar de gamma no responde:
#' cuánto se mueve el número que va al reporte.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()] con `est_rake` y
#'   `est_drmnar`.
#' @return Objeto [ggplot2::ggplot], o `NULL` si no hay filas DR-MNAR con
#'   estimaciones finitas.
#' @export
graficar_impacto_drmnar <- function(diagnostico) {
  bd <- diagnostico |>
    dplyr::filter(
      .data$decision == "DR-MNAR",
      is.finite(.data$est_rake), is.finite(.data$est_drmnar)
    )
  if (nrow(bd) == 0) return(NULL)

  bd <- bd |>
    dplyr::mutate(
      etiqueta = stringr::str_wrap(
        ifelse(is.na(.data$categoria), .data$pregunta,
               paste0(.data$pregunta, ": ", .data$categoria)), 28),
      pp = (.data$est_drmnar - .data$est_rake) * 100
    )

  largo <- bd |>
    tidyr::pivot_longer(
      cols = c("est_rake", "est_drmnar"),
      names_to = "estimador", values_to = "est"
    ) |>
    dplyr::mutate(
      estimador = ifelse(.data$estimador == "est_rake", "Raking (MAR)", "DR-MNAR")
    )

  ggplot(largo, aes(y = stats::reorder(.data$etiqueta, .data$est))) +
    geom_segment(
      data = bd,
      aes(x = .data$est_rake, xend = .data$est_drmnar,
          y = .data$etiqueta, yend = .data$etiqueta),
      linewidth = 1.1, color = COLOR_NEUTRO
    ) +
    geom_point(aes(x = .data$est, color = .data$estimador), size = 4.2) +
    geom_text(
      data = bd,
      aes(x = pmax(.data$est_rake, .data$est_drmnar), y = .data$etiqueta,
          label = sprintf("%+.1f pp", .data$pp)),
      hjust = -0.25, size = 4.3, fontface = "bold", color = COLOR_MORANT
    ) +
    scale_color_manual(
      values = c("Raking (MAR)" = COLOR_NEUTRO, "DR-MNAR" = COLOR_MORANT),
      name = NULL
    ) +
    scale_x_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0.06, 0.18))
    ) +
    labs(x = NULL, y = NULL) +
    tema_morant() +
    theme(legend.position = "bottom")
}
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: PASS, 2 pruebas.

- [ ] **Step 5: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-validacion-graficas.R \
        NAMESPACE man/graficar_impacto_drmnar.Rd
git commit -m "feat(norespuesta): cuánto se mueve el número al corregir

El diagnóstico ya calculaba est_rake, est_drmnar y diferencia, y ninguna lámina
los graficaba: el deck decía DÓNDE hay sesgo pero no CUÁNTO cuesta ignorarlo, que
es lo único que el tomador de decisiones puede usar. El dumbbell pone los dos
estimadores en la misma línea con el corrimiento en puntos porcentuales."
```

---

### Task 4: `graficar_precision_drmnar()`

**Files:**
- Modify: `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-validacion-graficas.R` (agregar)

**Interfaces:**
- Consumes: tibble de `estimar_drmnar()` (7 filas, columnas `modelo`, `est`, `ee`).
- Produces: `graficar_precision_drmnar(estimaciones)` → `ggplot` o `NULL`.

- [ ] **Step 1: Escribir el test que falla**

```r
test_that("graficar_precision_drmnar ordena los 7 estimadores y marca los MNAR", {
  sint <- crear_diseno_sintetico(n = 6000, gamma_y = 2, semilla = 41)
  est <- estimar_drmnar(
    diseno = sint$diseno, pregunta = "conoce_cand", covariables = "x",
    categoria = "Sí lo conoce", instrumento = "drmnar_z"
  )
  g <- graficar_precision_drmnar(est)
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$data), 7)
  expect_true("tipo" %in% names(g$data))
  expect_setequal(unique(g$data$tipo), c("MAR/Observado", "MNAR"))
})

test_that("graficar_precision_drmnar devuelve NULL sin errores estándar finitos", {
  est <- tibble::tibble(
    modelo = "DR-MNAR", est = 0.5, ee = NA_real_,
    pregunta = "x", categoria = "a", subconjunto = "Estado"
  )
  expect_null(graficar_precision_drmnar(est))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: FAIL, `could not find function "graficar_precision_drmnar"`.

- [ ] **Step 3: Implementar**

```r
#' Costo en precisión de los estimadores MNAR
#'
#' Barras del error estándar (sándwich, agrupado por UPM y estrato) de cada uno
#' de los siete estimadores de [estimar_drmnar()]. Corregir el sesgo de
#' selección no sale gratis: la estimación en dos etapas del modelo MNAR agrega
#' variabilidad, y esta lámina la deja explícita para que el intervalo del
#' reporte no se lea con más confianza de la que tiene.
#'
#' @param estimaciones Tibble de [estimar_drmnar()] (una pregunta).
#' @return Objeto [ggplot2::ggplot], o `NULL` si ningún error estándar es finito.
#' @export
graficar_precision_drmnar <- function(estimaciones) {
  orden <- c("Observado", "Weights-MAR", "Imput-MAR", "DR-MAR",
             "Weights-MNAR", "Imput-MNAR", "DR-MNAR")
  bd <- estimaciones |> dplyr::filter(is.finite(.data$ee))
  if (nrow(bd) == 0) return(NULL)

  bd <- bd |>
    dplyr::mutate(
      modelo = factor(.data$modelo, levels = rev(orden)),
      tipo = ifelse(grepl("MNAR", .data$modelo), "MNAR", "MAR/Observado")
    )

  ggplot(bd, aes(x = .data$modelo, y = .data$ee, fill = .data$tipo)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::percent(.data$ee, accuracy = 0.01)),
      hjust = -0.15, size = 4.3
    ) +
    scale_fill_manual(
      values = c("MAR/Observado" = COLOR_NEUTRO, "MNAR" = COLOR_MORANT),
      name = NULL
    ) +
    scale_y_continuous(
      labels = scales::percent,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    coord_flip() +
    labs(x = NULL, y = "Error estándar sándwich (agrupado por UPM y estrato)") +
    tema_morant() +
    theme(legend.position = "bottom")
}
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: PASS, 4 pruebas.

- [ ] **Step 5: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-validacion-graficas.R \
        NAMESPACE man/graficar_precision_drmnar.Rd
git commit -m "feat(norespuesta): el costo en precisión de corregir el sesgo

estimar_drmnar ya calculaba error estándar sándwich agrupado por UPM y estrato, y
el deck no lo reportaba en ningún lado. Los estimadores MNAR quitan sesgo pero
ensanchan el intervalo: sin esa lámina, el reporte se lee con más confianza de la
que tiene."
```

---

### Task 5: Control de multiplicidad

**Files:**
- Modify: `R/norespuesta_validacion.R`, `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-validacion.R`

**Interfaces:**
- Consumes: tibble de `diagnosticar_norespuesta()` (usa `z_stat`, `no_ignorable`).
- Produces:
  - `ajustar_multiplicidad_norespuesta(diagnostico, metodo = "BH")` → el
    diagnóstico más `p_valor`, `p_ajustado`, `sobrevive` (logical), con atributo
    `metodo`.
  - `graficar_multiplicidad_norespuesta(tabla, alfa = 0.05)` → `ggplot` o `NULL`.

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-norespuesta-validacion.R
fixture_multiplicidad <- function() {
  tibble::tibble(
    pregunta = paste0("p", 1:6),
    categoria = "Sí",
    subconjunto = "Estado",
    gamma_y = c(3.1, 2.2, 2.0, 0.4, 0.2, NA_real_),
    z_stat = c(5.0, 2.6, 2.1, 0.9, 0.4, NA_real_),
    no_ignorable = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
    decision = c(rep("DR-MNAR", 3), rep("Raking", 2), "Sin estimación")
  )
}

test_that("ajustar_multiplicidad_norespuesta reproduce stats::p.adjust", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  finitos <- is.finite(tab$z_stat)
  esperado <- stats::p.adjust(tab$p_valor[finitos], method = "BH")
  expect_equal(tab$p_ajustado[finitos], esperado)
  expect_true(all(is.na(tab$p_ajustado[!finitos])))
  expect_equal(attr(tab, "metodo"), "BH")
})

test_that("el ajuste nunca marca más que el diagnóstico crudo", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  expect_lte(sum(tab$sobrevive, na.rm = TRUE),
             sum(fixture_multiplicidad()$no_ignorable, na.rm = TRUE))
})

test_that("Holm es al menos tan estricto como BH", {
  bh <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  holm <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "holm")
  expect_lte(sum(holm$sobrevive, na.rm = TRUE), sum(bh$sobrevive, na.rm = TRUE))
})

test_that("graficar_multiplicidad_norespuesta compara crudo contra ajustado", {
  tab <- ajustar_multiplicidad_norespuesta(fixture_multiplicidad(), metodo = "BH")
  g <- graficar_multiplicidad_norespuesta(tab)
  expect_s3_class(g, "ggplot")
  expect_match(g$labels$subtitle, "azar")
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion.R")'`
Expected: FAIL, `could not find function "ajustar_multiplicidad_norespuesta"`.

- [ ] **Step 3: Implementar el cálculo en `R/norespuesta_validacion.R`**

```r
#' Control de multiplicidad de las detecciones de no ignorabilidad
#'
#' Cada pregunta x categoría x subconjunto del diagnóstico es una prueba de
#' hipótesis, así que con decenas de pruebas a `alfa = 0.05` se esperan varias
#' señales por puro azar. Esta función deriva el p-valor de dos colas del
#' `z_stat` del diagnóstico y lo ajusta por multiplicidad.
#'
#' El método se expone en vez de fijarse porque controlan cosas distintas:
#' `"BH"` controla la tasa de falso descubrimiento (FDR) y `"holm"` la tasa de
#' error por familia (FWER), que es más estricta. La diferencia entre uno y otro
#' es información que el lector técnico necesita, no un detalle interno.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()].
#' @param metodo Método de [stats::p.adjust()] (default `"BH"`).
#' @param alfa Nivel al que se declara sobreviviente (default 0.05).
#' @return El `diagnostico` con `p_valor`, `p_ajustado` y `sobrevive`, y el
#'   atributo `metodo`.
#' @export
ajustar_multiplicidad_norespuesta <- function(diagnostico, metodo = "BH",
                                              alfa = 0.05) {
  d <- diagnostico
  d$p_valor <- 2 * stats::pnorm(-abs(d$z_stat))
  finitos <- is.finite(d$p_valor)
  d$p_ajustado <- NA_real_
  if (any(finitos)) {
    d$p_ajustado[finitos] <- stats::p.adjust(d$p_valor[finitos], method = metodo)
  }
  d$sobrevive <- !is.na(d$p_ajustado) & d$p_ajustado < alfa
  attr(d, "metodo") <- metodo
  d
}
```

- [ ] **Step 4: Implementar la gráfica en `R/graficar_norespuesta_validacion.R`**

```r
#' Detecciones antes y después del control de multiplicidad
#'
#' Barras del número de detecciones crudas contra las que sobreviven al ajuste
#' de [ajustar_multiplicidad_norespuesta()], con la nota de cuántas señales se
#' esperan por azar. Es la lámina de cautela del deck: sin ella, un conteo crudo
#' de detecciones invita a leerlas todas como hallazgos firmes.
#'
#' @param tabla Salida de [ajustar_multiplicidad_norespuesta()].
#' @param alfa Nivel de significancia usado (default 0.05).
#' @return Objeto [ggplot2::ggplot], o `NULL` si no hay pruebas estimables.
#' @export
graficar_multiplicidad_norespuesta <- function(tabla, alfa = 0.05) {
  n_pruebas <- sum(is.finite(tabla$p_valor))
  if (n_pruebas == 0) return(NULL)

  metodo <- attr(tabla, "metodo") %||% "BH"
  n_crudo <- sum(tabla$p_valor < alfa, na.rm = TRUE)
  n_ajustado <- sum(tabla$sobrevive, na.rm = TRUE)
  esperados <- round(n_pruebas * alfa, 1)

  bd <- tibble::tibble(
    criterio = factor(
      c("Sin ajuste", paste0("Ajustado (", metodo, ")")),
      levels = c("Sin ajuste", paste0("Ajustado (", metodo, ")"))
    ),
    n = c(n_crudo, n_ajustado)
  )

  ggplot(bd, aes(x = .data$criterio, y = .data$n, fill = .data$criterio)) +
    geom_col(width = 0.5) +
    geom_hline(yintercept = esperados, linetype = "dashed", color = COLOR_NEUTRO) +
    geom_text(aes(label = .data$n), vjust = -0.4, size = 6, fontface = "bold") +
    scale_fill_manual(
      values = stats::setNames(c(COLOR_NEUTRO, COLOR_MORANT), levels(bd$criterio)),
      guide = "none"
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    labs(
      x = NULL, y = "detecciones",
      subtitle = stringr::str_wrap(sprintf(
        paste0("Con %d pruebas a alfa = %s se esperan ~%s señales por azar ",
               "(línea punteada). Sobreviven %d al ajuste por multiplicidad ",
               "(%s). Solo esas deben leerse como sesgo estructural; el resto ",
               "se confirma con más muestra en olas posteriores."),
        n_pruebas, alfa, esperados, n_ajustado, metodo), 95)
    ) +
    tema_morant()
}
```

- [ ] **Step 5: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion.R")'`
Expected: PASS, 4 pruebas.

- [ ] **Step 6: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/norespuesta_validacion.R R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-validacion.R NAMESPACE \
        man/ajustar_multiplicidad_norespuesta.Rd man/graficar_multiplicidad_norespuesta.Rd
git commit -m "feat(norespuesta): cuáles detecciones sobreviven al azar

Cada pregunta por categoría es una prueba: con 47 pruebas estimables al 5% se
esperan ~2.4 señales por puro azar, y el corte municipal de referencia marca 7.
De esas, 3 sobreviven a Benjamini-Hochberg y 1 a Holm. El método se expone en vez
de fijarse porque BH controla FDR y Holm controla FWER: la diferencia entre 3 y 1
es justo lo que el lector técnico necesita ver.

Esto vivía inline con officer en el estudio municipal; ahora es del paquete."
```

---

### Task 6: Validación con los desertores

**Files:**
- Modify: `R/norespuesta_validacion.R`, `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-desertores.R`

**Interfaces:**
- Consumes: `data.frame` del snapshot con `drmnar_z`, `drmnar_r` y las preguntas.
- Produces:
  - `comparar_desertores_norespuesta(bd, preguntas)` → tibble `pregunta`,
    `categoria`, `n_voluntario`, `n_desertor`, `p_voluntario`, `p_desertor`,
    `diferencia`, `p_valor`. `preguntas` es una lista nombrada
    `list(pregunta = categoria)`.
  - `graficar_desertores_norespuesta(tabla, diagnostico = NULL)` → `ggplot` o
    `NULL`.

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-norespuesta-desertores.R
test_that("el signo del contraste coincide con el signo de gamma simulado", {
  # gamma_y = 2 => quienes tienen y = 1 responden MÁS => entre los que
  # eligieron el módulo hay MÁS "Sí lo conoce" que entre los desertores
  sint <- crear_diseno_sintetico(n = 9000, gamma_y = 2, semilla = 77)
  bd <- sint$datos
  # los desertores del sintético traen y = 0 forzado; se repone la verdad
  bd$conoce_cand <- ifelse(sint$y_verdadera == 1, "Sí lo conoce", "No lo conoce")

  tab <- comparar_desertores_norespuesta(
    bd, preguntas = list("conoce_cand" = "Sí lo conoce")
  )
  expect_equal(nrow(tab), 1)
  expect_gt(tab$diferencia, 0)
  expect_true(is.finite(tab$p_valor))
  expect_equal(tab$n_voluntario + tab$n_desertor, sum(bd$drmnar_z == 1))
})

test_that("sin desertores devuelve cero filas con las columnas declaradas", {
  bd <- data.frame(
    drmnar_z = c(1, 1, 0), drmnar_r = c(1, 1, 1),
    conoce_cand = c("Sí lo conoce", "No lo conoce", "Sí lo conoce")
  )
  tab <- comparar_desertores_norespuesta(
    bd, preguntas = list("conoce_cand" = "Sí lo conoce")
  )
  expect_equal(nrow(tab), 0)
  expect_true(all(c("pregunta", "diferencia", "p_valor") %in% names(tab)))
})

test_that("graficar_desertores_norespuesta marca coincidencia con gamma", {
  tab <- tibble::tibble(
    pregunta = c("a", "b"), categoria = "Sí",
    n_voluntario = 100L, n_desertor = 400L,
    p_voluntario = c(0.60, 0.30), p_desertor = c(0.45, 0.40),
    diferencia = c(0.15, -0.10), p_valor = c(0.01, 0.20)
  )
  diag <- tibble::tibble(
    pregunta = c("a", "b"), categoria = "Sí",
    gamma_y = c(0.9, 0.8), decision = "DR-MNAR"
  )
  g <- graficar_desertores_norespuesta(tab, diag)
  expect_s3_class(g, "ggplot")
  expect_true("valida" %in% names(g$data))
  # "a" coincide en signo con gamma; "b" no
  expect_equal(g$data$valida[g$data$pregunta == "a"], TRUE)
  expect_equal(g$data$valida[g$data$pregunta == "b"], FALSE)
})

test_that("graficar_desertores_norespuesta devuelve NULL con tabla vacía", {
  vacia <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    n_voluntario = integer(0), n_desertor = integer(0),
    p_voluntario = numeric(0), p_desertor = numeric(0),
    diferencia = numeric(0), p_valor = numeric(0)
  )
  expect_null(graficar_desertores_norespuesta(vacia))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-desertores.R")'`
Expected: FAIL, `could not find function "comparar_desertores_norespuesta"`.

- [ ] **Step 3: Implementar el cálculo en `R/norespuesta_validacion.R`**

```r
#' Contraste con los desertores del filtro temático (continuum of resistance)
#'
#' Dentro del brazo de tratamiento, compara la proporción de cada categoría
#' entre quienes eligieron el módulo político (`drmnar_r == 1`) y quienes
#' desertaron al menú temático (`drmnar_r == 0`) pero aun así traen respuesta.
#'
#' Es una validación externa del modelo MNAR: si el signo de
#' `p_voluntario - p_desertor` coincide con el de `gamma_y`, dos rutas
#' independientes -el modelo de propensión y una diferencia de proporciones sin
#' modelo- llegan a la misma conclusión.
#'
#' @param bd `data.frame` del snapshot con `drmnar_z`, `drmnar_r` y las
#'   preguntas.
#' @param preguntas Lista nombrada `list(pregunta = categoria)`.
#' @return Tibble con `pregunta`, `categoria`, `n_voluntario`, `n_desertor`,
#'   `p_voluntario`, `p_desertor`, `diferencia` y `p_valor`. Cero filas si no
#'   hay desertores con respuesta.
#' @export
comparar_desertores_norespuesta <- function(bd, preguntas) {
  vacio <- tibble::tibble(
    pregunta = character(0), categoria = character(0),
    n_voluntario = integer(0), n_desertor = integer(0),
    p_voluntario = numeric(0), p_desertor = numeric(0),
    diferencia = numeric(0), p_valor = numeric(0)
  )
  if (!all(c("drmnar_z", "drmnar_r") %in% names(bd))) {
    stop("Corre primero preparar_variables_drmnar() sobre el snapshot.",
         call. = FALSE)
  }
  trat <- bd[bd$drmnar_z == 1, , drop = FALSE]
  if (nrow(trat) == 0) return(vacio)

  filas <- list()
  for (preg in names(preguntas)) {
    if (!preg %in% names(trat)) next
    cat_i <- preguntas[[preg]]
    val <- trat[[preg]]
    obs <- !is.na(val)
    vol <- obs & trat$drmnar_r == 1
    des <- obs & trat$drmnar_r == 0
    if (sum(vol) == 0 || sum(des) == 0) next

    y <- val %in% cat_i
    exitos <- c(sum(y & vol), sum(y & des))
    totales <- c(sum(vol), sum(des))
    pv <- tryCatch(
      stats::prop.test(exitos, totales)$p.value,
      error = function(e) NA_real_, warning = function(w) NA_real_
    )
    filas[[length(filas) + 1]] <- tibble::tibble(
      pregunta = preg,
      categoria = paste(cat_i, collapse = " | "),
      n_voluntario = as.integer(totales[1]),
      n_desertor = as.integer(totales[2]),
      p_voluntario = exitos[1] / totales[1],
      p_desertor = exitos[2] / totales[2],
      diferencia = exitos[1] / totales[1] - exitos[2] / totales[2],
      p_valor = pv
    )
  }
  if (length(filas) == 0) return(vacio)
  dplyr::bind_rows(filas)
}
```

- [ ] **Step 4: Implementar la gráfica en `R/graficar_norespuesta_validacion.R`**

```r
#' Validación del diagnóstico con los desertores del filtro
#'
#' Dumbbell de la proporción entre quienes eligieron el módulo político y
#' quienes desertaron. Cuando se pasa el diagnóstico, marca por pregunta si el
#' signo del contraste COINCIDE con el de `gamma_y`: la coincidencia es
#' validación cruzada del modelo MNAR y la discrepancia se muestra tal cual, no
#' se oculta.
#'
#' @param tabla Salida de [comparar_desertores_norespuesta()].
#' @param diagnostico Tibble de [diagnosticar_norespuesta()] (opcional).
#' @return Objeto [ggplot2::ggplot], o `NULL` si la tabla viene vacía.
#' @export
graficar_desertores_norespuesta <- function(tabla, diagnostico = NULL) {
  if (nrow(tabla) == 0) return(NULL)

  bd <- tabla
  bd$valida <- NA
  if (!is.null(diagnostico) && nrow(diagnostico) > 0) {
    g <- diagnostico$gamma_y[match(bd$pregunta, diagnostico$pregunta)]
    bd$valida <- ifelse(is.finite(g), sign(bd$diferencia) == sign(g), NA)
  }
  bd$marca <- dplyr::case_when(
    is.na(bd$valida) ~ "Sin gamma comparable",
    bd$valida ~ "Coincide con gamma",
    .default = "Discrepa de gamma"
  )
  bd$etiqueta <- stringr::str_wrap(
    paste0(bd$pregunta, ": ", bd$categoria), 28)

  largo <- bd |>
    tidyr::pivot_longer(
      cols = c("p_voluntario", "p_desertor"),
      names_to = "grupo", values_to = "p"
    ) |>
    dplyr::mutate(
      grupo = ifelse(.data$grupo == "p_voluntario",
                     "Eligió el módulo político", "Desertó al menú temático")
    )

  ggplot(largo, aes(y = stats::reorder(.data$etiqueta, .data$p))) +
    geom_segment(
      data = bd,
      aes(x = .data$p_desertor, xend = .data$p_voluntario,
          y = .data$etiqueta, yend = .data$etiqueta,
          color = .data$marca),
      linewidth = 1.1
    ) +
    geom_point(aes(x = .data$p, shape = .data$grupo), size = 4, color = COLOR_NEUTRO) +
    scale_color_manual(
      values = c("Coincide con gamma" = COLOR_MORANT,
                 "Discrepa de gamma" = "#8A5E06",
                 "Sin gamma comparable" = COLOR_NEUTRO),
      name = NULL
    ) +
    scale_shape_manual(
      values = c("Eligió el módulo político" = 16, "Desertó al menú temático" = 1),
      name = NULL
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL) +
    tema_morant() +
    theme(legend.position = "bottom", legend.box = "vertical")
}
```

- [ ] **Step 5: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-desertores.R")'`
Expected: PASS, 4 pruebas.

- [ ] **Step 6: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/norespuesta_validacion.R R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-desertores.R NAMESPACE \
        man/comparar_desertores_norespuesta.Rd man/graficar_desertores_norespuesta.Rd
git commit -m "feat(norespuesta): los desertores validan el modelo por fuera

Los desertores del filtro temático traen respuesta política y nadie los
contrastaba: en el corte municipal son 2,904 contra 677 que eligieron el módulo.
Comparar las proporciones entre ambos grupos es una diferencia de medias SIN
modelo, así que cuando su signo coincide con el de gamma, dos rutas
independientes llegan a la misma conclusión.

La discrepancia se dibuja tal cual, no se oculta: es un resultado legítimo del
diagnóstico y el lector tiene que verlo."
```

---

### Task 7: Balance del instrumento

**Files:**
- Modify: `R/norespuesta_validacion.R`, `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-instrumento.R`

**Interfaces:**
- Consumes: `data.frame` del snapshot con `drmnar_z` y las covariables.
- Produces:
  - `evaluar_instrumento_norespuesta(bd, covariables)` → tibble `covariable`,
    `nivel`, `prop_z0`, `prop_z1`, `dme`.
  - `graficar_balance_instrumento(tabla, umbral = 0.1)` → `ggplot` o `NULL`.

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-norespuesta-instrumento.R
test_that("con Z al azar todas las DME caen dentro de la banda", {
  set.seed(11)
  n <- 6000
  bd <- data.frame(
    drmnar_z = stats::rbinom(n, 1, 0.5),
    sexo = sample(c("Hombre", "Mujer"), n, replace = TRUE),
    rango_edad = sample(c("18-24", "25-59", "60+"), n, replace = TRUE)
  )
  tab <- evaluar_instrumento_norespuesta(bd, c("sexo", "rango_edad"))
  expect_true(all(c("covariable", "nivel", "prop_z0", "prop_z1", "dme") %in% names(tab)))
  expect_true(all(abs(tab$dme) < 0.1))
})

test_that("una covariable correlacionada con Z sale fuera de la banda", {
  set.seed(12)
  n <- 6000
  x <- stats::rbinom(n, 1, 0.5)
  bd <- data.frame(
    drmnar_z = stats::rbinom(n, 1, ifelse(x == 1, 0.85, 0.15)),
    sesgada = ifelse(x == 1, "Alto", "Bajo")
  )
  tab <- evaluar_instrumento_norespuesta(bd, "sesgada")
  expect_true(any(abs(tab$dme) > 0.1))
})

test_that("graficar_balance_instrumento dibuja el love plot con su banda", {
  tab <- tibble::tibble(
    covariable = c("sexo", "sexo"), nivel = c("Hombre", "Mujer"),
    prop_z0 = c(0.48, 0.52), prop_z1 = c(0.50, 0.50),
    dme = c(0.04, -0.04)
  )
  g <- graficar_balance_instrumento(tab)
  expect_s3_class(g, "ggplot")
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% geoms)
})

test_that("evaluar_instrumento_norespuesta ignora covariables ausentes", {
  bd <- data.frame(drmnar_z = c(0, 1, 0, 1), sexo = c("H", "M", "H", "M"))
  tab <- evaluar_instrumento_norespuesta(bd, c("sexo", "no_existe"))
  expect_setequal(unique(tab$covariable), "sexo")
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-instrumento.R")'`
Expected: FAIL, `could not find function "evaluar_instrumento_norespuesta"`.

- [ ] **Step 3: Implementar el cálculo en `R/norespuesta_validacion.R`**

```r
#' Balance de covariables entre brazos del instrumento
#'
#' Diferencia de medias estandarizada (DME) de cada nivel de cada covariable
#' entre el brazo de tratamiento y el de control:
#' `(p1 - p0) / sqrt((p1(1-p1) + p0(1-p0)) / 2)`.
#'
#' Sostiene el supuesto de identificabilidad del diseño: el brazo se asignó al
#' azar, así que las covariables deben quedar balanceadas. Una DME grande
#' señalaría que el menú temático atrae o repele selectivamente y que el
#' instrumento no es válido para identificar `gamma_y`.
#'
#' @param bd `data.frame` del snapshot con `drmnar_z` y las covariables.
#' @param covariables Vector de nombres de covariables. Las que no estén en `bd`
#'   se ignoran.
#' @return Tibble con `covariable`, `nivel`, `prop_z0`, `prop_z1` y `dme`.
#' @export
evaluar_instrumento_norespuesta <- function(bd, covariables) {
  vacio <- tibble::tibble(
    covariable = character(0), nivel = character(0),
    prop_z0 = numeric(0), prop_z1 = numeric(0), dme = numeric(0)
  )
  if (!"drmnar_z" %in% names(bd)) {
    stop("Corre primero preparar_variables_drmnar() sobre el snapshot.",
         call. = FALSE)
  }
  z <- bd$drmnar_z
  if (sum(z == 1) == 0 || sum(z == 0) == 0) return(vacio)

  filas <- list()
  for (cv in intersect(covariables, names(bd))) {
    val <- as.character(bd[[cv]])
    for (nivel in sort(unique(stats::na.omit(val)))) {
      ind <- val == nivel
      p1 <- mean(ind[z == 1], na.rm = TRUE)
      p0 <- mean(ind[z == 0], na.rm = TRUE)
      s <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
      filas[[length(filas) + 1]] <- tibble::tibble(
        covariable = cv, nivel = nivel,
        prop_z0 = p0, prop_z1 = p1,
        dme = if (is.finite(s) && s > 0) (p1 - p0) / s else 0
      )
    }
  }
  if (length(filas) == 0) return(vacio)
  dplyr::bind_rows(filas)
}
```

- [ ] **Step 4: Implementar la gráfica en `R/graficar_norespuesta_validacion.R`**

```r
#' Love plot del balance del instrumento aleatorizado
#'
#' Diferencias de medias estandarizadas por nivel de covariable, con la banda de
#' referencia +/- `umbral`. Todo dentro de la banda respalda que el brazo se
#' asignó al azar; lo que se sale hay que explicarlo antes de leer cualquier
#' `gamma_y` estimado con ese instrumento.
#'
#' @param tabla Salida de [evaluar_instrumento_norespuesta()].
#' @param umbral Banda de referencia (default 0.1).
#' @return Objeto [ggplot2::ggplot], o `NULL` si la tabla viene vacía.
#' @export
graficar_balance_instrumento <- function(tabla, umbral = 0.1) {
  if (nrow(tabla) == 0) return(NULL)

  bd <- tabla |>
    dplyr::mutate(
      etiqueta = paste0(.data$covariable, ": ", .data$nivel),
      fuera = abs(.data$dme) > umbral
    )
  n_fuera <- sum(bd$fuera)

  ggplot(bd, aes(x = .data$dme,
                 y = stats::reorder(.data$etiqueta, .data$dme),
                 color = .data$fuera)) +
    geom_vline(xintercept = 0, color = COLOR_NEUTRO) +
    geom_vline(xintercept = c(-umbral, umbral), linetype = "dashed",
               color = COLOR_NEUTRO) +
    geom_point(size = 4) +
    scale_color_manual(
      values = c("FALSE" = COLOR_NEUTRO, "TRUE" = COLOR_MORANT),
      labels = c("FALSE" = "Balanceada", "TRUE" = "Fuera de banda"),
      name = NULL
    ) +
    labs(
      x = "Diferencia de medias estandarizada (tratamiento - control)",
      y = NULL,
      subtitle = stringr::str_wrap(sprintf(
        paste0("El brazo se asigna al azar, así que las covariables deben ",
               "quedar balanceadas: %d de %d niveles se salen de la banda de ",
               "+/-%.2f. Una covariable fuera de banda significaría que el menú ",
               "temático atrae selectivamente y que el instrumento no ",
               "identifica gamma."),
        n_fuera, nrow(bd), umbral), 95)
    ) +
    tema_morant() +
    theme(legend.position = "bottom")
}
```

- [ ] **Step 5: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-instrumento.R")'`
Expected: PASS, 4 pruebas.

- [ ] **Step 6: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/norespuesta_validacion.R R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-instrumento.R NAMESPACE \
        man/evaluar_instrumento_norespuesta.Rd man/graficar_balance_instrumento.Rd
git commit -m "feat(norespuesta): evidencia de que el instrumento es válido

Todo gamma estimado descansa en que el brazo se asignó al azar y el menú temático
no atrae selectivamente. El deck lo daba por hecho sin mostrarlo. El love plot de
diferencias estandarizadas por nivel de covariable, con banda de +/-0.1, es la
evidencia: va antes del diagnóstico porque si el instrumento no se sostiene, no
hay nada que leer después."
```

---

### Task 8: `graficar_heterogeneidad_norespuesta()`

**Files:**
- Modify: `R/graficar_norespuesta_validacion.R`
- Test: `tests/testthat/test-norespuesta-validacion-graficas.R` (agregar)

**Interfaces:**
- Consumes: diagnóstico con más de un valor en `subconjunto`.
- Produces: `graficar_heterogeneidad_norespuesta(diagnostico)` → `ggplot` o
  `NULL` si hay un solo subconjunto.

- [ ] **Step 1: Escribir el test que falla**

```r
test_that("graficar_heterogeneidad_norespuesta marca los cambios de signo", {
  diag <- tibble::tibble(
    pregunta = rep(c("aprob_pm", "chapulineo"), each = 2),
    categoria = "Sí",
    subconjunto = rep(c("Morena", "Oposición"), 2),
    gamma_y = c(1.2, -0.9, 0.4, 0.5),
    inf = c(0.6, -1.5, -0.2, -0.1),
    sup = c(1.8, -0.3, 1.0, 1.1),
    no_ignorable = c(TRUE, TRUE, FALSE, FALSE),
    decision = c("DR-MNAR", "DR-MNAR", "Raking", "Raking")
  )
  g <- graficar_heterogeneidad_norespuesta(diag)
  expect_s3_class(g, "ggplot")
  expect_equal(nrow(g$data), 4)
  # aprob_pm cambia de signo entre subgrupos, chapulineo no
  expect_true(all(g$data$cambia_signo[g$data$pregunta == "aprob_pm"]))
  expect_false(any(g$data$cambia_signo[g$data$pregunta == "chapulineo"]))
})

test_that("graficar_heterogeneidad_norespuesta devuelve NULL con un solo subconjunto", {
  expect_null(graficar_heterogeneidad_norespuesta(fixture_impacto()))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: FAIL, `could not find function "graficar_heterogeneidad_norespuesta"`.

- [ ] **Step 3: Implementar**

```r
#' Heterogeneidad de gamma entre subgrupos
#'
#' Compara `gamma_y` por subconjunto para cada pregunta y destaca las que
#' CAMBIAN DE SIGNO entre subgrupos. Bailey documenta que la no respuesta no
#' ignorable suele ser heterogénea: los sesgos de grupos opuestos se cancelan y
#' la población general da `gamma ~ 0` aunque dentro de cada grupo sea severo.
#'
#' @param diagnostico Tibble de [diagnosticar_norespuesta()] corrido con
#'   `subconjuntos`.
#' @return Objeto [ggplot2::ggplot], o `NULL` si hay un solo subconjunto.
#' @export
graficar_heterogeneidad_norespuesta <- function(diagnostico) {
  bd <- diagnostico |> dplyr::filter(is.finite(.data$gamma_y))
  if (nrow(bd) == 0 || length(unique(bd$subconjunto)) < 2) return(NULL)

  bd <- bd |>
    dplyr::group_by(.data$pregunta) |>
    dplyr::mutate(
      cambia_signo = dplyr::n_distinct(sign(.data$gamma_y)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      etiqueta = stringr::str_wrap(
        ifelse(is.na(.data$categoria), .data$pregunta,
               paste0(.data$pregunta, ": ", .data$categoria)), 28)
    )

  n_cambia <- dplyr::n_distinct(bd$pregunta[bd$cambia_signo])

  ggplot(bd, aes(x = .data$gamma_y,
                 y = stats::reorder(.data$etiqueta, .data$gamma_y))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = COLOR_NEUTRO) +
    geom_line(aes(group = .data$etiqueta, color = .data$cambia_signo),
              linewidth = 1) +
    geom_point(aes(shape = .data$subconjunto, color = .data$cambia_signo),
               size = 3.6) +
    scale_color_manual(
      values = c("FALSE" = COLOR_NEUTRO, "TRUE" = COLOR_MORANT),
      labels = c("FALSE" = "Mismo signo", "TRUE" = "Cambia de signo"),
      name = NULL
    ) +
    scale_shape_discrete(name = NULL) +
    labs(
      x = expression(gamma[Y] ~ "por subgrupo"), y = NULL,
      subtitle = stringr::str_wrap(sprintf(
        paste0("%d pregunta(s) cambian de signo entre subgrupos: ahí el sesgo ",
               "se cancela en la población general y el diagnóstico global la ",
               "declara ignorable aunque dentro de cada grupo no lo sea."),
        n_cambia), 95)
    ) +
    tema_morant() +
    theme(legend.position = "bottom", legend.box = "vertical")
}
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-norespuesta-validacion-graficas.R")'`
Expected: PASS, 6 pruebas.

- [ ] **Step 5: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/graficar_norespuesta_validacion.R \
        tests/testthat/test-norespuesta-validacion-graficas.R \
        NAMESPACE man/graficar_heterogeneidad_norespuesta.Rd
git commit -m "feat(norespuesta): el sesgo que se cancela entre subgrupos

Bailey documenta que la no respuesta no ignorable suele ser heterogénea: los
sesgos de grupos opuestos se cancelan y la población general da gamma ~ 0 aunque
dentro de cada grupo sea severo. La lámina destaca justo las preguntas que CAMBIAN
DE SIGNO entre subgrupos, que son las que el diagnóstico global declara ignorables
por la razón equivocada."
```

---

### Task 9: `armar_deck_norespuesta()`

**Files:**
- Modify: `R/deck_norespuesta.R`
- Test: `tests/testthat/test-deck-norespuesta.R`

**Interfaces:**
- Consumes: todas las funciones de las tareas 2–8, más las seis gráficas base
  que ya existen (`graficar_flujo_norespuesta`, `graficar_tabla_covariables`,
  `graficar_diagnostico_norespuesta`, `graficar_comparacion_estimadores`,
  `graficar_pesos_drmnar`, `graficar_decision_norespuesta`).
- Produces: `armar_deck_norespuesta(...)` → ruta del `.pptx` (invisible).

- [ ] **Step 1: Escribir el test que falla**

```r
# tests/testthat/test-deck-norespuesta.R
test_that("armar_deck_norespuesta produce un pptx con las láminas del corte", {
  sint <- crear_diseno_sintetico(n = 7000, gamma_y = 2, semilla = 91)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x", instrumento = "drmnar_z"
  )
  salida <- file.path(tempdir(), "deck_norespuesta.pptx")

  ruta <- armar_deck_norespuesta(
    diseno = sint$diseno, bundle = bundle,
    plantilla = NULL, salida = salida,
    titulo = "Diagnóstico de no respuesta",
    subtitulo = "Prueba sintética",
    layout = "Title and Content", master = "Office Theme"
  )

  expect_true(file.exists(ruta))
  # portada + al menos flujo, covariables, gamma, impacto, multiplicidad y decisión
  expect_gte(length(officer::read_pptx(ruta)), 6)
})

test_that("armar_deck_norespuesta omite las láminas sin insumo en vez de reventar", {
  # gamma = 0 => ninguna pregunta activa DR-MNAR => sin impacto ni pesos
  sint <- crear_diseno_sintetico(n = 5000, gamma_y = 0, semilla = 92)
  bundle <- generar_diseno_drmnar(
    diseno = sint$diseno,
    preguntas = list("conoce_cand" = "Sí lo conoce"),
    covariables = "x", instrumento = "drmnar_z"
  )
  salida <- file.path(tempdir(), "deck_norespuesta_vacio.pptx")

  expect_no_error(
    ruta <- armar_deck_norespuesta(
      diseno = sint$diseno, bundle = bundle,
      plantilla = NULL, salida = salida,
      titulo = "Sin detecciones", subtitulo = "Prueba sintética",
      layout = "Title and Content", master = "Office Theme"
    )
  )
  expect_true(file.exists(ruta))
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-deck-norespuesta.R")'`
Expected: FAIL, `could not find function "armar_deck_norespuesta"`.

- [ ] **Step 3: Implementar**

```r
#' Deck de diagnóstico de no respuesta no ignorable (pptx)
#'
#' Arma el deck completo del diagnóstico DR-MNAR desde el paquete: doce láminas
#' que van del diseño y su validez al dictamen de ponderación, con las lecturas
#' de `gamma` en lenguaje de negocio.
#'
#' Antes de existir esta función cada estudio portaba su propio script de ~230
#' líneas; ya habían divergido entre sí y dos láminas se armaban con `officer`
#' inline, fuera del contrato de que todo el graficado sale del paquete. Lo que
#' cambia entre estudios entra por parámetro.
#'
#' Toda lámina cuyo insumo salga vacío se OMITE en vez de reventar, para que el
#' deck sirva a estudios en distintas etapas de campo.
#'
#' @param diseno Objeto `survey::svydesign` del estudio.
#' @param bundle Objeto `diseno_drmnar` de [generar_diseno_drmnar()].
#' @param plantilla Ruta al `.pptx` plantilla; `NULL` usa la default de officer.
#' @param salida Ruta del `.pptx` a escribir.
#' @param titulo,subtitulo Textos de la portada.
#' @param preguntas_clave Preguntas headline que se grafican como REFERENCIA
#'   aunque su no respuesta sea ignorable (control de que los siete estimadores
#'   coincidan cuando gamma ~ 0).
#' @param subgrupos Lista nombrada de vectores lógicos para la lámina de
#'   heterogeneidad. `NULL` (default) omite la lámina; encendida recalcula el
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

  pptx <- if (is.null(plantilla)) officer::read_pptx() else officer::read_pptx(plantilla)

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

  # 2. flujo del instrumento
  pptx <- agregar(pptx, "Flujo del instrumento aleatorizado (protocolo de campo)",
                  .try_null(graficar_flujo_norespuesta(resumen_flujo_norespuesta(bd))))

  # 3. balance del instrumento (validez antes que diagnóstico)
  pptx <- agregar(pptx, "Balance del instrumento — supuesto de exclusión",
                  .try_null(graficar_balance_instrumento(
                    evaluar_instrumento_norespuesta(bd, covs))))

  # 4. covariables del ajuste
  doc <- catalogo_covariables %||% bundle$covariables_doc
  if (!is.null(doc) && nrow(doc) > 0) {
    pptx <- agregar(pptx, "Covariables del ajuste doblemente robusto",
                    .try_null(graficar_tabla_covariables(
                      doc, n_ef, bundle$covariables_umbral %||% 1200L,
                      bundle$covariables_ricas %||% covs)))
  }

  # 5. caterpillar de gamma, solo las DR-MNAR
  dr_diag <- diag |>
    dplyr::filter(.data$decision == "DR-MNAR", is.finite(.data$gamma_y)) |>
    dplyr::arrange(dplyr::desc(abs(.data$gamma_y))) |>
    utils::head(top_gammas)
  pptx <- agregar(pptx, sprintf(
    "Gamma de no ignorabilidad — preguntas DR-MNAR (%d de %d)",
    nrow(dr_diag), nrow(diag)),
    if (nrow(dr_diag) > 0) .try_null(graficar_diagnostico_norespuesta(dr_diag)) else NULL)

  # 6. impacto práctico
  pptx <- agregar(pptx, "Impacto práctico: Raking vs DR-MNAR",
                  .try_null(graficar_impacto_drmnar(diag)))

  # 7-8. comparación de estimadores y precisión, por pregunta
  preguntas_dr <- bundle$decision$pregunta[bundle$decision$decision == "DR-MNAR"]
  clave <- intersect(preguntas_clave %||% character(0), diag$pregunta)
  for (cod in unique(c(clave, preguntas_dr))) {
    cat_ref <- diag$categoria[diag$pregunta == cod][1]
    est <- .try_null(estimar_drmnar(
      diseno, pregunta = cod, covariables = covs,
      instrumento = instrumento, categoria = cat_ref))
    if (is.null(est)) next
    etq <- if (cod %in% preguntas_dr) "DR-MNAR" else "referencia (ignorable)"
    pptx <- agregar(pptx, sprintf("Comparación de estimadores — %s = \"%s\"  [%s]",
                                  cod, cat_ref, etq),
                    .try_null(graficar_comparacion_estimadores(est)))
    if (cod %in% preguntas_dr) {
      pptx <- agregar(pptx, sprintf("Precisión (EE sándwich) — %s", cod),
                      .try_null(graficar_precision_drmnar(est)))
    }
  }

  # 9. pesos de propensión inversa
  if (length(preguntas_dr) > 0) {
    cat_pesos <- diag$categoria[diag$pregunta == preguntas_dr[1]][1]
    pptx <- agregar(pptx, paste0("Pesos de propensión inversa — ", preguntas_dr[1]),
                    .try_null(graficar_pesos_drmnar(stats::weights(
                      diseno_para_pregunta(bundle, preguntas_dr[1],
                                           categoria = cat_pesos)))))
  }

  # 10. validación con desertores
  preguntas_lista <- stats::setNames(
    as.list(diag$categoria[diag$decision == "DR-MNAR"]),
    diag$pregunta[diag$decision == "DR-MNAR"])
  if (length(preguntas_lista) > 0) {
    pptx <- agregar(pptx, "Validación con los desertores del filtro temático",
                    .try_null(graficar_desertores_norespuesta(
                      comparar_desertores_norespuesta(bd, preguntas_lista), diag)))
  }

  # 11. multiplicidad
  pptx <- agregar(pptx, "¿Cuáles detecciones sobreviven al azar?",
                  .try_null(graficar_multiplicidad_norespuesta(
                    ajustar_multiplicidad_norespuesta(
                      diag, metodo = metodo_multiplicidad))))

  # 12. dictamen
  pptx <- agregar(pptx, "Decisión por pregunta: DR-MNAR vs Raking",
                  .try_null(graficar_decision_norespuesta(diag)))

  # opcional: heterogeneidad por subgrupo
  if (!is.null(subgrupos)) {
    diag_sub <- .try_null(diagnosticar_norespuesta(
      diseno = diseno,
      preguntas = stats::setNames(as.list(diag$categoria), diag$pregunta),
      covariables = covs, instrumento = instrumento, subconjuntos = subgrupos))
    pptx <- agregar(pptx, "Heterogeneidad de gamma por subgrupo",
                    .try_null(graficar_heterogeneidad_norespuesta(diag_sub)))
  }

  dir.create(dirname(salida), showWarnings = FALSE, recursive = TRUE)
  print(pptx, target = salida)
  invisible(salida)
}

# Coloca un elemento en su placeholder; si la plantilla no lo tiene, deja la
# lámina como está en vez de tumbar el deck completo.
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

# El paquete declara R (>= 2.10), así que no se puede asumir el `%||%` de base
# (llegó en R 4.4). Se define aquí porque el armador lo usa en cada default que
# sale del bundle.
`%||%` <- function(a, b) if (is.null(a)) b else a
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-deck-norespuesta.R")'`
Expected: PASS, 2 pruebas.

- [ ] **Step 5: Correr toda la suite**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS, sin fallos.

- [ ] **Step 6: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/deck_norespuesta.R tests/testthat/test-deck-norespuesta.R \
        NAMESPACE man/armar_deck_norespuesta.Rd
git commit -m "feat(norespuesta): el deck se arma desde el paquete

El armado no vivía en encuestar: la estatal jul-2026 (256 líneas) y la municipal
(229) eran forks del mismo script, ya divergidos. Ese fork tuvo costo real —dos
láminas con officer inline fuera del contrato, y una llamada a una función
inexistente que reventó el paso 8 en producción— porque el paquete no podía
proteger un armado que no era suyo.

armar_deck_norespuesta() arma las doce láminas y recibe por parámetro todo lo que
cambia entre estudios. El script de cada encuesta queda en ~20 líneas de
configuración, y una encuesta nueva obtiene el deck sin portar nada. Las láminas
sin insumo se omiten en vez de reventar, para servir a estudios en distintas
etapas de campo."
```

---

### Task 10: Métodos delegadores en la clase R6 `NoRespuesta`

**Files:**
- Modify: `R/graficar_norespuesta.R` (la definición de `NoRespuesta`)
- Test: `tests/testthat/test-graficar-norespuesta.R` (agregar)

**Interfaces:**
- Consumes: las funciones de las tareas 2–8.
- Produces: `$lectura()`, `$grafica_impacto()`, `$grafica_precision(pregunta,
  categoria)`, `$multiplicidad(metodo)`, `$grafica_balance()` y
  `$grafica_desertores(preguntas)` sobre el objeto `NoRespuesta`, para que el
  hub `Resultados` las alcance sin cambiar su API.

- [ ] **Step 1: Escribir el test que falla**

```r
test_that("NoRespuesta delega las funciones de validación", {
  sint <- crear_diseno_sintetico(n = 7000, gamma_y = 2, semilla = 43)
  nr <- NoRespuesta$new(
    diseno = sint$diseno, covariables = "x", instrumento = "drmnar_z"
  )
  nr$diagnostico(preguntas = list("conoce_cand" = "Sí lo conoce"))

  lec <- nr$lectura()
  expect_true(all(c("estado", "texto") %in% names(lec)))
  expect_equal(lec$estado[1], "sobre_representacion")

  mult <- nr$multiplicidad()
  expect_true(all(c("p_valor", "p_ajustado", "sobrevive") %in% names(mult)))

  expect_s3_class(nr$grafica_balance(), "ggplot")
  expect_s3_class(
    nr$grafica_precision(pregunta = "conoce_cand", categoria = "Sí lo conoce"),
    "ggplot"
  )
})

test_that("los delegadores exigen un diagnóstico previo", {
  sint <- crear_diseno_sintetico(n = 3000, gamma_y = 0, semilla = 44)
  nr <- NoRespuesta$new(
    diseno = sint$diseno, covariables = "x", instrumento = "drmnar_z"
  )
  expect_error(nr$lectura(), "diagnostico")
})
```

- [ ] **Step 2: Correr el test y ver que falla**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-graficar-norespuesta.R")'`
Expected: FAIL, `attempt to apply non-function` en `nr$lectura()`.

- [ ] **Step 3: Implementar los métodos dentro de `NoRespuesta$public`**

Insertar después del método `descriptivos`:

```r
    ,
    #' @description Lectura de gamma en lenguaje de negocio
    #'  ([lectura_norespuesta()]).
    #' @param diagnostico Diagnóstico (default: el último cacheado).
    lectura = function(diagnostico = NULL) {
      bd <- if (is.null(diagnostico)) self$ultimo_diagnostico else diagnostico
      if (is.null(bd)) stop("Corre primero $diagnostico(preguntas = ...).")
      lectura_norespuesta(bd)
    },
    #' @description Impacto práctico ([graficar_impacto_drmnar()]).
    #' @param diagnostico Diagnóstico (default: el último cacheado).
    grafica_impacto = function(diagnostico = NULL) {
      bd <- if (is.null(diagnostico)) self$ultimo_diagnostico else diagnostico
      if (is.null(bd)) stop("Corre primero $diagnostico(preguntas = ...).")
      graficar_impacto_drmnar(bd)
    },
    #' @description Costo en precisión ([graficar_precision_drmnar()]).
    #' @param pregunta Pregunta a estimar.
    #' @param categoria Categoría que define y = 1.
    #' @param ... Argumentos para [NoRespuesta$estimacion()].
    grafica_precision = function(pregunta, categoria = NULL, ...) {
      graficar_precision_drmnar(
        self$estimacion(pregunta = pregunta, categoria = categoria, ...)
      )
    },
    #' @description Control de multiplicidad
    #'  ([ajustar_multiplicidad_norespuesta()]).
    #' @param metodo Método de [stats::p.adjust()].
    #' @param diagnostico Diagnóstico (default: el último cacheado).
    multiplicidad = function(metodo = "BH", diagnostico = NULL) {
      bd <- if (is.null(diagnostico)) self$ultimo_diagnostico else diagnostico
      if (is.null(bd)) stop("Corre primero $diagnostico(preguntas = ...).")
      ajustar_multiplicidad_norespuesta(bd, metodo = metodo)
    },
    #' @description Balance del instrumento ([graficar_balance_instrumento()]).
    #' @param covariables Covariables (default las de la clase).
    grafica_balance = function(covariables = self$covariables) {
      graficar_balance_instrumento(
        evaluar_instrumento_norespuesta(self$diseno$variables, covariables)
      )
    },
    #' @description Validación con desertores
    #'  ([graficar_desertores_norespuesta()]).
    #' @param preguntas Lista nombrada `list(pregunta = categoria)`.
    #' @param diagnostico Diagnóstico (default: el último cacheado).
    grafica_desertores = function(preguntas, diagnostico = NULL) {
      bd <- if (is.null(diagnostico)) self$ultimo_diagnostico else diagnostico
      graficar_desertores_norespuesta(
        comparar_desertores_norespuesta(self$diseno$variables, preguntas), bd
      )
    }
```

- [ ] **Step 4: Correr el test y ver que pasa**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-graficar-norespuesta.R")'`
Expected: PASS.

- [ ] **Step 5: Documentar y commitear**

```bash
Rscript -e 'devtools::document()'
git add R/graficar_norespuesta.R tests/testthat/test-graficar-norespuesta.R man/NoRespuesta.Rd
git commit -m "feat(norespuesta): la clase R6 alcanza las funciones de validación

NoRespuesta es la puerta del hub Resultados al módulo de no respuesta. Sin
delegadores, las funciones nuevas solo se podían llamar sueltas y el hub se
quedaba con el diagnóstico base. Los métodos reusan el diagnóstico ya cacheado
en vez de recalcularlo."
```

---

### Task 11: Versión y NEWS

**Files:**
- Modify: `DESCRIPTION`, `NEWS.md`

**Interfaces:**
- Consumes: todo lo anterior.
- Produces: `encuestar` 2.2.0 instalable.

- [ ] **Step 1: Subir la versión**

En `DESCRIPTION`, cambiar `Version: 2.1.1` por `Version: 2.2.0`.

- [ ] **Step 2: Escribir la entrada de NEWS**

Agregar al inicio de `NEWS.md`:

```markdown
# encuestar 2.2.0

## Deck de diagnóstico DR-MNAR reutilizable

* `armar_deck_norespuesta()` arma el deck completo desde el paquete. Cada
  estudio portaba su propio script de ~230 líneas, ya divergidos entre sí; ahora
  quedan ~20 líneas de configuración.
* `lectura_norespuesta()` traduce `gamma` a lenguaje de negocio en tres estados:
  sobre-representación (el raking sobreestima), ocultamiento (subestima) e
  ignorable.
* `graficar_impacto_drmnar()` grafica cuánto se mueve la estimación al corregir,
  con `est_rake` y `est_drmnar` que el diagnóstico ya calculaba.
* `graficar_precision_drmnar()` reporta el error estándar sándwich por
  estimador: corregir el sesgo ensancha el intervalo.
* `ajustar_multiplicidad_norespuesta()` y `graficar_multiplicidad_norespuesta()`
  controlan por multiplicidad (BH o Holm) las detecciones de no ignorabilidad.
* `comparar_desertores_norespuesta()` y `graficar_desertores_norespuesta()`
  validan los `gamma` contra los desertores del filtro temático, sin modelo.
* `evaluar_instrumento_norespuesta()` y `graficar_balance_instrumento()`
  documentan el balance de covariables por brazo (supuesto de exclusión).
* `graficar_heterogeneidad_norespuesta()` destaca las preguntas cuyo `gamma`
  cambia de signo entre subgrupos.
* `graficar_decision_norespuesta()` reemplaza la lámina de decisiones que cada
  estudio armaba con `officer` inline.
```

- [ ] **Step 3: Verificar que el paquete instala y la suite pasa**

Run: `Rscript -e 'devtools::document(); devtools::test()'`
Expected: PASS, sin fallos.

- [ ] **Step 4: Commit y push**

```bash
git add DESCRIPTION NEWS.md man/ NAMESPACE
git commit -m "chore(norespuesta): encuestar 2.2.0

Doce funciones nuevas del módulo de no respuesta y el armador del deck."
git push
```

- [ ] **Step 5: Sacar el PR #302 de borrador**

```bash
gh pr ready 302
```

---

### Task 12: Migrar el estudio municipal

**Files:**
- Modify: `../enc_edomex_municipal_jul_2026/R/press_norespuesta.R` (229 → ~40 líneas)
- Modify: `../enc_edomex_municipal_jul_2026/R/configuracion.R`

**Interfaces:**
- Consumes: `armar_deck_norespuesta()` de `encuestar` 2.2.0.
- Produces: el pptx en `entregables/<fecha>/pptx/norespuesta_municipal_edomex.pptx`.

> Este task corre en el repo del estudio, en la rama `feat/deck-drmnar-validacion`.
> **Solo stagear los archivos de este task**: el working tree tiene cambios
> ajenos en `R/bloques/bloque_10.R` y `R/graficas_editables.R`.

- [ ] **Step 1: Instalar el paquete desde la rama**

Run: `Rscript -e 'remotes::install_local("../encuestar", force = TRUE, upgrade = "never")'`
Expected: instala `encuestar` 2.2.0.

Nota: mientras el PR #302 no se mergee, el estudio corre contra un install
LOCAL, no contra master. Decirlo explícitamente en el reporte.

- [ ] **Step 2: Reescribir el script como configuración**

```r
# ===============================================================
# press_norespuesta.R — Deck de DIAGNÓSTICO DR-MNAR (municipal 309)
#
# El armado vive en `encuestar::armar_deck_norespuesta()`: este script es SOLO
# la configuración del estudio. Antes eran 229 líneas de cableado de láminas,
# forkeadas del script de la estatal; esa duplicación ya había costado dos
# láminas con officer inline y una llamada a una función inexistente que reventó
# el paso 8 en producción.
#
# Correr desde la raíz del proyecto (después del paso 2):
#   Rscript R/press_norespuesta.R
# ===============================================================
source("R/configuracion.R")
suppressPackageStartupMessages({
  library(dplyr); library(encuestar)
})
options(survey.lonely.psu = "adjust")

`%||%` <- function(a, b) if (is.null(a)) b else a

ultimo <- function(patron) {
  f <- sort(list.files("bd", pattern = patron, full.names = TRUE),
            decreasing = TRUE)
  stopifnot(length(f) > 0); f[1]
}
diseno <- readRDS(ultimo("edomex_municipal_snapshot_.*\\.rds$"))
bundle <- readRDS(ultimo("edomex_municipal_drmnar_.*\\.rds$"))

.fecha_corte <- {
  d <- suppressWarnings(as.Date(diseno$variables$dia %||% diseno$variables$Date))
  if (all(is.na(d))) Sys.Date() else max(d, na.rm = TRUE)
}

SALIDA <- file.path("entregables", format(Sys.Date(), "%Y-%m-%d"), "pptx",
                    "norespuesta_municipal_edomex.pptx")

ruta <- armar_deck_norespuesta(
  diseno = diseno,
  bundle = bundle,
  plantilla = file.path("insumos", "Plantilla_MORANT.pptx"),
  salida = SALIDA,
  titulo = "Diagnóstico de no respuesta (DR-MNAR)",
  subtitulo = "Encuesta municipal · Estado de México · jul 2026",
  preguntas_clave = c("aprob_claudia", "aprob_delfina", "aprob_pm"),
  subgrupos = DRMNAR_SUBGRUPOS,
  nota = sprintf(
    "n = %s efectivas · instrumento: componente_50 · covariables: %s · corte: %s",
    format(nrow(diseno$variables), big.mark = ","),
    paste(bundle$covariables, collapse = " + "),
    format(.fecha_corte, "%d-%b-%Y"))
)

cat(sprintf("\nOK: deck de diagnóstico DR-MNAR -> %s (%d láminas)\n",
            ruta, length(officer::read_pptx(ruta))))
```

- [ ] **Step 3: Agregar el opt-in en `R/configuracion.R`**

Agregar al final del archivo:

```r
# Subgrupos para la lámina de heterogeneidad de gamma del deck DR-MNAR.
# NULL = apagada (default). Encenderla recalcula el diagnóstico completo por
# cada subgrupo, que es caro: se prende a propósito cuando se quiere explorar
# si el sesgo se cancela entre grupos opuestos, no en cada corrida.
#
# Ejemplo:
#   DRMNAR_SUBGRUPOS <- list(
#     "Morena"    = diseno$variables$partido == "MORENA",
#     "Oposición" = diseno$variables$partido != "MORENA"
#   )
DRMNAR_SUBGRUPOS <- NULL
```

- [ ] **Step 4: Correr el paso 8 y contar láminas**

Run: `Rscript R/press_norespuesta.R`
Expected: imprime la ruta y un conteo de láminas MAYOR al del deck anterior
(el anterior tenía portada + flujo + covariables + gamma + comparativas + pesos
+ decisión).

- [ ] **Step 5: Correr los tests del contrato**

Run: `Rscript R/run_todo.R --paso 9`
Expected: `contrato` y `entregables` en verde. `congruencia` puede seguir rojo
por los 6 folios con GPS cruzado, que es previo a este trabajo.

- [ ] **Step 6: Commit**

```bash
git add R/press_norespuesta.R R/configuracion.R
git commit -m "feat(paso 8): el deck de DR-MNAR se arma desde el paquete

press_norespuesta.R pasa de 229 líneas de cableado de láminas a configuración del
estudio. El cableado era un fork del script de la estatal y ya había costado dos
láminas con officer inline -fuera del contrato de que todo el graficado sale de
encuestar- y una llamada a una función inexistente que reventaba el paso 8 en la
última lámina, después de calcular todo el deck.

De paso entran las seis láminas de validación e impacto: balance del instrumento,
impacto práctico, precisión sándwich, desertores, multiplicidad y las lecturas de
gamma en lenguaje de negocio.

DRMNAR_SUBGRUPOS queda apagado por default: encenderlo recalcula el diagnóstico
por cada subgrupo."
```

---

### Task 13: Cerrar el ciclo contra master

**Files:** ninguno (verificación).

- [ ] **Step 1: Esperar el merge del PR #302 y reinstalar desde master**

Run: `Rscript -e 'remotes::install_github("morant-consultores/encuestar@master", upgrade = "never")'`
Expected: instala `encuestar` 2.2.0 desde master.

- [ ] **Step 2: Re-correr el paso 8 contra master**

Run: `Rscript R/press_norespuesta.R`
Expected: mismo conteo de láminas que en el Task 12, ahora sin install local.

- [ ] **Step 3: Regenerar el corte y commitear**

```bash
git add entregables/
git commit -m "chore(deck): regenera el corte con las láminas de validación DR-MNAR"
git push -u origin feat/deck-drmnar-validacion
```

- [ ] **Step 4: Abrir el PR del estudio**

```bash
gh pr create --base main --head feat/deck-drmnar-validacion \
  --title "feat(paso 8): el deck de DR-MNAR se arma desde encuestar"
```

---

## Notas de ejecución

- **El estudio estatal no se toca.** Cuando su responsable decida migrarlo, su
  `encuesta/02_press_norespuesta.R` colapsa a la misma llamada del Task 12.
- **Working tree ajeno.** El repo del estudio tiene cambios en
  `R/bloques/bloque_10.R` y `R/graficas_editables.R` que no son de este trabajo:
  nunca hacer `git add .` ahí.
- **Orden obligado.** Tasks 1–11 en `encuestar`; 12–13 en el estudio, y el 13
  después del merge del PR #302.
