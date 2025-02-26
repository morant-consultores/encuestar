% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clase_respuestas.R
\name{Respuestas}
\alias{Respuestas}
\title{Clasoe Respuestas}
\description{
La clase \code{Respuestas} tiene como objetivo coonsolidar, limpiar y preparar las
respuestas de los individuos entrevistados en campo.
}
\section{Super class}{
\code{\link[encuestar:Encuesta]{encuestar::Encuesta}} -> \code{Respuestas}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{eliminadas}}{El campo \code{eliminadas} contiene el \code{\link[=tibble]{tibble()}} de entrevistas eliminadas por
auditoría.}

\item{\code{cluster_corregido}}{El campo \code{cluster_corregido} contiene el \code{\link[=tibble]{tibble()}} de entrevistas cuyo
cluster haya sido corregido por geolocalización.}

\item{\code{base}}{El campo \code{base} contiene el \code{\link[=tibble]{tibble()}} de las respuestas serán utilizadas para construir
el diseño muestral de la encuesta.}

\item{\code{catalogo}}{El campo \code{catálogo} contiene el catálogo de variables en el cual se definen aqueelas
que son necesarias y suficientes para construir un diseño muestral.}

\item{\code{n}}{El campo \code{n} está en desuso.}

\item{\code{m}}{El campo \code{m} está en desuso.}

\item{\code{sin_coordenadas}}{El campo \code{sin_coordenadas} contiene el \code{\link[=tibble]{tibble()}} de, en caso de existir,
entrevistas que no tienen una geolicalización válida.}

\item{\code{mantener_falta_coordenadas}}{\code{LOGICAL} determina si las entrevistas que no tienen una
geolocalización válida se descartan o no.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Respuestas-new}{\code{Respuestas$new()}}
\item \href{#method-Respuestas-nombres}{\code{Respuestas$nombres()}}
\item \href{#method-Respuestas-q_patron}{\code{Respuestas$q_patron()}}
\item \href{#method-Respuestas-categorias}{\code{Respuestas$categorias()}}
\item \href{#method-Respuestas-respuestas_sin_seleccion}{\code{Respuestas$respuestas_sin_seleccion()}}
\item \href{#method-Respuestas-eliminar_auditoria_telefonica}{\code{Respuestas$eliminar_auditoria_telefonica()}}
\item \href{#method-Respuestas-corregir_respuestas}{\code{Respuestas$corregir_respuestas()}}
\item \href{#method-Respuestas-crear_variablesSecundarias}{\code{Respuestas$crear_variablesSecundarias()}}
\item \href{#method-Respuestas-coordenadas_faltantes}{\code{Respuestas$coordenadas_faltantes()}}
\item \href{#method-Respuestas-eliminar_falta_coordenadas}{\code{Respuestas$eliminar_falta_coordenadas()}}
\item \href{#method-Respuestas-correccion_cluster}{\code{Respuestas$correccion_cluster()}}
\item \href{#method-Respuestas-eliminar_fuera_muestra}{\code{Respuestas$eliminar_fuera_muestra()}}
\item \href{#method-Respuestas-calcular_distancia}{\code{Respuestas$calcular_distancia()}}
\item \href{#method-Respuestas-eliminar_faltantes_diseno}{\code{Respuestas$eliminar_faltantes_diseno()}}
\item \href{#method-Respuestas-vars_diseno}{\code{Respuestas$vars_diseno()}}
\item \href{#method-Respuestas-clone}{\code{Respuestas$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="encuestar" data-topic="Encuesta" data-id="error_muestral_maximo"><a href='../../encuestar/html/Encuesta.html#method-Encuesta-error_muestral_maximo'><code>encuestar::Encuesta$error_muestral_maximo()</code></a></span></li>
<li><span class="pkg-link" data-pkg="encuestar" data-topic="Encuesta" data-id="exportar_entregable"><a href='../../encuestar/html/Encuesta.html#method-Encuesta-exportar_entregable'><code>encuestar::Encuesta$exportar_entregable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="encuestar" data-topic="Encuesta" data-id="simular_surveytogo"><a href='../../encuestar/html/Encuesta.html#method-Encuesta-simular_surveytogo'><code>encuestar::Encuesta$simular_surveytogo()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-new"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-new}{}}}
\subsection{Method \code{new()}}{
Se reciben los diferentes insumos relacionados a la encuesta para estandarizar
las respuestas que formarán parte del diseÑo muestral.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$new(
  base = NULL,
  catalogo = NA,
  encuesta = NULL,
  muestra_completa = NULL,
  mantener_falta_coordenadas = FALSE,
  patron = NA,
  nivel = NULL,
  var_n = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{base}}{\code{\link[=tibble]{tibble()}} que contiene la base de datos de respuestas de las personas entrevistadas.}

\item{\code{catalogo}}{\code{\link[=tibble]{tibble()}} que contiene el catalogo de variables.}

\item{\code{encuesta}}{Clase de jerarquía mayor.}

\item{\code{muestra_completa}}{Objeto de formato personalizado generado por la paquetería \code{muestrear}.
Contiene el diseño muestral completo de la encuesta.}

\item{\code{mantener_falta_coordenadas}}{\code{LOGICAL} determina si las entrevistas que no tienen una
geolocalización válida se descartan o no.}

\item{\code{patron}}{Cadena de caracteres que se removerán de todas las respuestas recibidas en
campo.}

\item{\code{nivel}}{Valor tipo entero que indica el número de etapas de muestro.}

\item{\code{var_n}}{Valor tipo caracter que indica el nombre de la variable asociado al último
nivel de la etapa de muestreo.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-nombres"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-nombres}{}}}
\subsection{Method \code{nombres()}}{
Determina si la base de respuestas contiene todas las variables que, de acuerdo
al diccionario, deben ser procesadas.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$nombres(catalogo_variables, bd, diccionario)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{catalogo_variables}}{\code{\link[=tibble]{tibble()}} que contiene el catálogo de variables.}

\item{\code{bd}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{diccionario}}{Diccionario (o codebook) del cuestionario de la encuesta.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-q_patron"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-q_patron}{}}}
\subsection{Method \code{q_patron()}}{
Quita cadenas de texto de las respuetas de campo heredadas del cuestionario
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$q_patron(bd, dicc, patron)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bd}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{dicc}}{Diccionario (o codebook) del cuestionario de la encuesta.}

\item{\code{patron}}{Cadena o cadenas de texto que se desean quitar de las respuestas de campo}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-categorias"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-categorias}{}}}
\subsection{Method \code{categorias()}}{
Determina diferencias entre las respuestas recolectadas en campo y las esperadas
de acuerdo al diccionario
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$categorias(bd, diccionario)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bd}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{diccionario}}{Diccionario (o codebook) del cuestionario de la encuesta.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-respuestas_sin_seleccion"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-respuestas_sin_seleccion}{}}}
\subsection{Method \code{respuestas_sin_seleccion()}}{
Determina las respuestas esperadas que no han sido seleccionadas por los
entrevistados
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$respuestas_sin_seleccion(bd, diccionario)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bd}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{diccionario}}{Diccionario (o codebook) del cuestionario de la encuesta.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-eliminar_auditoria_telefonica"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-eliminar_auditoria_telefonica}{}}}
\subsection{Method \code{eliminar_auditoria_telefonica()}}{
Descarta las entrevistas que, de acuerdo a la base de auditoria telefónica,
deben ser descartadas.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$eliminar_auditoria_telefonica(auditoria_telefonica)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{auditoria_telefonica}}{\code{\link[=tibble]{tibble()}} que contiene las entrevistas que, de acuerdo a
auditoría, se van a eliminar del registro.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-corregir_respuestas"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-corregir_respuestas}{}}}
\subsection{Method \code{corregir_respuestas()}}{
Corrige las respuestas de los entrevistados de acuerdo a la base de correcciones
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$corregir_respuestas(respuestas, bd_correcciones_raw)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{respuestas}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{bd_correcciones_raw}}{\code{\link[=tibble]{tibble()}} que contiene la relación de entrevista, variable en la
cual está incorrecto el registro y valor correcto de la respuesta}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-crear_variablesSecundarias"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-crear_variablesSecundarias}{}}}
\subsection{Method \code{crear_variablesSecundarias()}}{
Crea variables que, por defecto, se usan en la producción y entrega de resultados
de Morant Consultores
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$crear_variablesSecundarias(diccionario)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{diccionario}}{Diccionario (o codebook) del cuestionario de la encuesta.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-coordenadas_faltantes"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-coordenadas_faltantes}{}}}
\subsection{Method \code{coordenadas_faltantes()}}{
Determina las entrevistas con variables de geolocalización no válidas
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$coordenadas_faltantes()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-eliminar_falta_coordenadas"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-eliminar_falta_coordenadas}{}}}
\subsection{Method \code{eliminar_falta_coordenadas()}}{
Descarta y registra las entrevistas con variables de geolocalización no válidas
y comunica al usuario esa operación
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$eliminar_falta_coordenadas()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-correccion_cluster"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-correccion_cluster}{}}}
\subsection{Method \code{correccion_cluster()}}{
Con base en la geolocalización de la entrevista, determina el cluster (o sección)
más cercano y le asigna el número de ese cluster. Para el diseno muestral esto implica
que la entrevista fue levantada en el cluster más cercano a la geolocalización.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$correccion_cluster(base, shp, mantener, nivel, var_n)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{base}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{shp}}{Campo de la clase encuesta. contiene toda la información cartográfica de la
encuesta}

\item{\code{mantener}}{Campo de la clase encuesta. Las entrevistas mantendrán el registro del
cluster en el cual hayan sido levantadas sin importas si hay otro cluster más cercano.}

\item{\code{nivel}}{Valor tipo entero que indica el número de etapas de muestro}

\item{\code{var_n}}{var_n Valor tipo caracter que indica el nombre de la variable asociado al último
nivel de la etapa de muestreo.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-eliminar_fuera_muestra"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-eliminar_fuera_muestra}{}}}
\subsection{Method \code{eliminar_fuera_muestra()}}{
Descarta las entrevistas cuyo cuyo cluster levantado no pertenezca a la lista
de clusters en muestra y notifica al usuario.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$eliminar_fuera_muestra(respuestas, muestra, nivel, var_n)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{respuestas}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{muestra}}{Contiene el diseño muestral completo de la encuesta.}

\item{\code{nivel}}{Valor tipo entero que indica el número de etapas de muestro}

\item{\code{var_n}}{var_n Valor tipo caracter que indica el nombre de la variable asociado al último
nivel de la etapa de muestreo.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-calcular_distancia"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-calcular_distancia}{}}}
\subsection{Method \code{calcular_distancia()}}{
Calcula la distancia (en metros) entre la geolocalización donde se lvantó la
entrevista y el cluster más cercano que encuentre.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$calcular_distancia(base, encuesta, muestra, var_n, nivel)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{base}}{\code{\link[=tibble]{tibble()}} que contiene las respuestas recolectadas en campo}

\item{\code{encuesta}}{Clase Encuesta de jerarquía mayor}

\item{\code{muestra}}{Contiene el diseño muestral completo de la encuesta.}

\item{\code{var_n}}{var_n Valor tipo caracter que indica el nombre de la variable asociado al último
nivel de la etapa de muestreo.}

\item{\code{nivel}}{Valor tipo entero que indica el número de etapas de muestro}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-eliminar_faltantes_diseno"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-eliminar_faltantes_diseno}{}}}
\subsection{Method \code{eliminar_faltantes_diseno()}}{
Descartar entrevistas que por alguna razón u otra no se calculó la información
relacionada a su diseño muestral
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$eliminar_faltantes_diseno()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-vars_diseno"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-vars_diseno}{}}}
\subsection{Method \code{vars_diseno()}}{
Agrega las variables relacionadas al diseno muestral
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$vars_diseno(muestra, var_n, tipo_encuesta)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{muestra}}{Contiene el diseño muestral completo de la encuesta.}

\item{\code{var_n}}{var_n Valor tipo caracter que indica el nombre de la variable asociado al último
nivel de la etapa de muestreo.}

\item{\code{tipo_encuesta}}{Campo de la clase Encuesta que determina el tipo de encuesta levantada.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Respuestas-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Respuestas-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Respuestas$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
