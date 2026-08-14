# Deck DR-MNAR reutilizable: validación, impacto y lectura automática

Fecha: 2026-08-14 · Paquete `encuestar` · verificado contra el corte municipal
EdoMex 2026-08-13

## Problema

El deck de diagnóstico DR-MNAR responde **dónde** hay no respuesta no ignorable,
pero no responde las preguntas que un lector técnico hace enseguida:

1. ¿**Cuánto** se mueve el número al corregir? El bundle ya trae `est_rake`,
   `est_drmnar` y `diferencia`, y ninguna lámina los grafica.
2. ¿**Cuánto cuesta** en precisión? `estimar_drmnar` ya calcula error estándar
   sándwich agrupado por UPM y estrato, y no se reporta.
3. ¿El instrumento `Z` es **válido**? No hay evidencia de que el menú de deserte
   no atraiga selectivamente por covariables.
4. ¿Hay **validación externa** de los γ? Los desertores del brazo de tratamiento
   traen respuesta política y nadie los contrasta.
5. ¿Las detecciones son **reales o ruido**? Con ~48 pruebas a α = 0.05 se esperan
   ~2.4 señales por azar.
6. ¿Qué **significa** cada γ en lenguaje de negocio? El signo se lee a mano.

### El problema de fondo: el deck está duplicado

El armado de láminas **no vive en el paquete**. Cada estudio tiene su copia:

| Proyecto | Script | Líneas |
|---|---|---|
| EdoMex estatal jul-2026 | `encuesta/02_press_norespuesta.R` | 256 |
| EdoMex municipal jul-2026 | `R/press_norespuesta.R` | 229 |

El segundo declara en su encabezado ser un *"Port del `02_press_norespuesta.R` de
la estatal"*. Ya divergieron, y cada encuesta nueva hereda el fork. Consecuencias
observadas:

- La lámina de decisiones y la de multiplicidad se arman con `officer` **inline
  en el proyecto**, incumpliendo la regla de que todo el graficado del deck salga
  de funciones de `encuestar`.
- Una llamada a una función inexistente (`graficar_decision_norespuesta`) vivió
  en el script municipal hasta que reventó el paso 8 en producción; el paquete no
  podía protegerlo porque el armado no era suyo.

**Por eso el armador del deck entra al paquete.** Sin eso, agregar seis láminas
solo multiplica el trabajo de portar.

## Alcance

Once funciones de contenido, un armador de deck, dos migraciones de láminas que
hoy son `officer` inline, y una lámina opt-in. **Fuera de alcance**: cambiar el
motor de estimación, el diseño muestral, o el deck de resultados (`mvf_press` de
`morantvizfactory`).

## Diseño

### Armador del deck

```r
armar_deck_norespuesta(diseno, bundle, plantilla, salida,
                       titulo, subtitulo,
                       preguntas_clave = NULL,
                       subgrupos = NULL,
                       catalogo_covariables = NULL,
                       top_gammas = 30,
                       layout = "una_graf",
                       master = "Tema de Office")
```

Arma las 12 láminas y devuelve la ruta del `.pptx`. Todo lo que hoy difiere entre
la estatal y la municipal entra como parámetro: plantilla, salida, títulos,
preguntas clave de referencia, catálogo de covariables, layout del master.

El script de cada estudio pasa de ~230 líneas a ~20 de configuración, y una
encuesta nueva obtiene el deck completo sin portar nada. Como el armado vive en
el paquete, ya no hay dónde meter `officer` inline en un proyecto.

`preguntas_clave` conserva el comportamiento actual: preguntas headline que se
grafican como **referencia** aunque su no respuesta sea ignorable, para mostrar
que los siete estimadores coinciden cuando γ ≈ 0.

### Funciones de contenido (todas exportadas)

| Función | Entrada | Salida | Gap |
|---|---|---|---|
| `graficar_decision_norespuesta(diagnostico, alfa)` | tibble de `diagnosticar_norespuesta` | ggplot | migración lámina 12 |
| `lectura_norespuesta(diagnostico)` | íd. | tibble `pregunta`, `categoria`, `estado`, `texto` | Sec. 1C |
| `graficar_impacto_drmnar(diagnostico)` | íd. | ggplot dumbbell | 1 |
| `graficar_precision_drmnar(estimaciones)` | tibble de `estimar_drmnar` | ggplot | 5 |
| `ajustar_multiplicidad_norespuesta(diagnostico, metodo)` | íd. | tibble + `p_valor`, `p_ajustado`, `sobrevive` | 6 |
| `graficar_multiplicidad_norespuesta(tabla, alfa)` | salida de la anterior | ggplot | 6 |
| `comparar_desertores_norespuesta(bd, preguntas)` | snapshot | tibble `pregunta`, `categoria`, `p_voluntario`, `p_desertor`, `diferencia`, `ee`, `inf`, `sup`, `p_valor` | 3 |
| `graficar_desertores_norespuesta(tabla, diagnostico)` | íd. + diagnóstico | ggplot | 3 |
| `evaluar_instrumento_norespuesta(bd, covariables)` | snapshot | tibble `covariable`, `nivel`, `prop_z0`, `prop_z1`, `dme` | 4 |
| `graficar_balance_instrumento(tabla, umbral)` | íd. | ggplot love plot | 4 |
| `graficar_heterogeneidad_norespuesta(diagnostico)` | diagnóstico con >1 subconjunto | ggplot | 2 |

#### Contrato de `lectura_norespuesta`

Tres estados excluyentes, derivados del γ y su significancia:

| Condición | `estado` | Texto |
|---|---|---|
| γ > 0 y el IC excluye 0 | `sobre_representacion` | Quienes están en la categoría responden **más**; el raking **sobreestima** el indicador |
| γ < 0 y el IC excluye 0 | `sub_representacion` | Quienes están en la categoría responden **menos** (ocultamiento); el raking **subestima** el indicador |
| IC incluye 0, γ no estimable o sin convergencia | `ignorable` | No respuesta ignorable; el raking es adecuado y **más eficiente** (menor varianza) |

El texto se arma desde los datos (pregunta, categoría, γ, corrimiento en puntos
porcentuales), nunca desde una tabla fija, para que no se desincronice del corte.

#### Contrato de `comparar_desertores_norespuesta`

Contrasta, **dentro del brazo de tratamiento** (`drmnar_z == 1`), a quienes
eligieron el módulo político (`drmnar_r == 1`) contra quienes desertaron al menú
temático (`drmnar_r == 0`) y aun así traen respuesta política. En el corte
municipal del 13-ago son 677 contra 2,904. Diferencia de proporciones con prueba
no paramétrica.

El valor está en el **contraste de signo** con γ: si
`sign(p_voluntario - p_desertor) == sign(gamma_y)`, el modelo MNAR queda validado
de forma independiente del modelo de propensión. `graficar_desertores_norespuesta`
recibe el diagnóstico justamente para marcar coincidencia o discrepancia por
pregunta.

Si en un estudio los desertores no traen respuesta política, la función devuelve
cero filas y el armador **omite la lámina** en vez de reventar.

#### Contrato de `evaluar_instrumento_norespuesta`

Diferencia de medias estandarizada (DME) de cada covariable entre brazos, con
banda de referencia ±0.1. Responde al supuesto de identificabilidad: `Z` se
asignó al azar, así que las covariables deben estar balanceadas; una DME grande
señalaría que el menú atrae selectivamente y que el instrumento no es válido.

### Reorganización de archivos

`R/graficar_norespuesta.R` (391 líneas) se parte para no duplicar su tamaño:

```
R/graficar_norespuesta.R             gráficas del diagnóstico base (las 6 actuales)
R/norespuesta_lectura.R              lectura_norespuesta
R/norespuesta_validacion.R           cálculo: multiplicidad, desertores, instrumento
R/graficar_norespuesta_validacion.R  sus gráficas + impacto + precisión + heterogeneidad
R/deck_norespuesta.R                 armar_deck_norespuesta
```

La clase R6 `NoRespuesta` se queda donde está y gana métodos delegadores para las
funciones nuevas, para que el hub `Resultados` las alcance sin cambiar su API.

### Deck resultante

```
1  Portada
2  Flujo del instrumento                      existente
3  Balance del instrumento — exclusión        NUEVO (gap 4)
4  Covariables del ajuste                     existente
5  Gamma caterpillar + lectura de signos      existente + lectura automática
6  Impacto práctico: Raking vs DR-MNAR        NUEVO (gap 1)
7  Comparación de 7 estimadores × pregunta    existente
8  Precisión: EE sándwich por estimador       NUEVO (gap 5)
9  Pesos 1/π                                  existente
10 Validación con desertores                  NUEVO (gap 3)
11 Multiplicidad (BH) sobre las detecciones   MIGRACIÓN del officer inline (gap 6)
12 Decisión final DR-MNAR vs Raking           MIGRACIÓN del officer inline
(+ Heterogeneidad de γ por subgrupo          opt-in, apagado por default)
```

Narrativa: diseño y validez (1–4) → diagnóstico e impacto (5–6) → profundización
y costo (7–8) → validación robusta (9–11) → dictamen (12).

Toda lámina cuyo insumo salga vacío se **omite**, no revienta: el armador debe
servir a estudios en distintas etapas de campo.

### Opt-in de heterogeneidad

`armar_deck_norespuesta(subgrupos = ...)` recibe una lista nombrada de vectores
lógicos, `NULL` por default. Apagado, el deck cuesta lo mismo que hoy; encendido,
corre `diagnosticar_norespuesta(subconjuntos = ...)` y agrega la lámina.

Va apagado porque recalcula todas las preguntas por cada subgrupo. Su razón de
ser es la que documenta Bailey: el sesgo puede cancelarse en la población general
y ser severo dentro de un partido o un municipio.

## Pruebas

Testthat sobre `crear_diseno_sintetico` / `simular_drmnar`, que ya existen en
`helper-drmnar.R` y generan datos con γ conocido:

- `lectura_norespuesta`: un caso por estado — γ>0 significativo, γ<0
  significativo, IC que incluye 0 — más γ no estimable (`NA`) y sin convergencia.
- `comparar_desertores_norespuesta`: sobre datos simulados con γ conocido, el
  signo de la diferencia coincide con el signo de γ; caso sin desertores devuelve
  cero filas sin reventar.
- `evaluar_instrumento_norespuesta`: con `Z` asignado al azar todas las DME caen
  dentro de ±0.1; con `Z` correlacionado a propósito con una covariable, esa
  covariable sale fuera de la banda.
- `ajustar_multiplicidad_norespuesta`: BH nunca marca más que sin ajuste; con
  p-valores conocidos reproduce `stats::p.adjust`; Holm es más estricto que BH.
- Gráficas: devuelven `ggplot` y no revientan con cero filas.
- `armar_deck_norespuesta`: sobre el diseño sintético y la plantilla de `inst/`,
  produce un `.pptx` con el número de láminas esperado, y omite las láminas cuyo
  insumo va vacío.

## Plan de entrega

Dos PRs, `encuestar` primero porque los estudios dependen de él.

`encuestar`, rama `feat/norespuesta-validacion-bailey` desde `origin/master`:

```
1  feat: graficar_decision_norespuesta          migración lámina 12   [hecho]
2  refactor: parte graficar_norespuesta.R
3  feat: lectura_norespuesta
4  feat: graficar_impacto_drmnar
5  feat: graficar_precision_drmnar
6  feat: multiplicidad (ajustar_ + graficar_)
7  feat: desertores (comparar_ + graficar_)
8  feat: instrumento (evaluar_ + graficar_balance_)
9  feat: graficar_heterogeneidad_norespuesta
10 feat: armar_deck_norespuesta
11 chore: 2.1.1 → 2.2.0 + NEWS
```

Estudio municipal, rama `feat/deck-drmnar-validacion`:

```
1  feat(paso 8): press_norespuesta.R llama al armador del paquete
2  feat(config): subgrupos DR-MNAR opt-in
3  chore(deck): regenera el corte
```

El estudio estatal (`../enc_edomex_estatal_jul_2026`) **no se toca en esta
entrega**; el PR del paquete documenta cómo colapsar su
`encuesta/02_press_norespuesta.R` a la llamada nueva cuando su responsable
decida migrarlo.

### Dependencia entre repos

Los estudios instalan `encuestar` de GitHub, no lo clonan. Para verificar de
punta a punta antes del merge se usa `remotes::install_local("../encuestar")`
desde la rama; una vez mergeado el PR del paquete, se reinstala desde `master` y
se vuelve a correr el paso 8. **Mientras un estudio corra contra un install local
en vez de master, se dice explícitamente en el reporte.**

## Riesgos

- **Falso positivo en el corte de referencia.** De las 48 estimaciones del corte
  municipal 2026-08-13, 47 tienen γ estimable y 7 quedan marcadas; al 5% se
  esperan ~2.4 por azar. Sobreviven **3 a Benjamini-Hochberg** y **1 a Holm**. Por
  eso `ajustar_multiplicidad_norespuesta` expone el método: BH controla FDR y
  Holm controla FWER, y la diferencia entre 3 y 1 es exactamente la que el lector
  técnico tiene que ver.
- **γ = −3.12 de `calif_horacio`** tiene IC muy ancho (−5.19, −1.05) y n chico en
  la categoría. La lámina de precisión lo va a exhibir; es lo correcto.
- **Los desertores pueden no validar.** Si el signo del contraste no coincide con
  γ en varias preguntas, la lámina 10 muestra discrepancia. Es un resultado
  legítimo y se reporta como tal, no se esconde.
- **El armador es superficie pública nueva.** Su firma queda con parámetros
  nombrados y valores por default para que agregar láminas después no rompa a los
  estudios que ya lo llamen.
