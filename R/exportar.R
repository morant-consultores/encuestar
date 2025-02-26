if(getRversion() >= "2.15.1")  utils::globalVariables(c("text", "Date", "Latitude", "Longitude"))

#' Title
#'
#' @param self
#' @param carpeta
#' @param agregar
#' @param quitar
#'
#' @return
#' @export
#'
#' @examples

exportar_bd <- function(self, carpeta, agregar, quitar){

  # Variables provenientes del cuestionario
  vars <- cuestionario |>
    pull(llaves)

  # Base ordenada con fecha, ubicación, variables del cuestionario, variables adicionales opcionales
  # y sin variables eliminadas
  compartir <- self$respuestas$base %>%
    select(Fecha = Date, Latitud = Latitude, Longitud = Longitude,
           all_of(vars),
           all_of(agregar)) %>%
    select(-any_of(quitar))

  eliminar <- names(self$respuestas$base)[is.na(match(names(self$respuestas$base), names(compartir)))]

  continuar <- yesno::yesno2(glue::glue("Desea eliminar las siguientes variables?: \n {paste(eliminar, collapse = ', ')}"),
                             yes = "Si", no = "No")

  if(continuar){

    compartir %>% readr::write_excel_csv(glue::glue("{carpeta}/bd.csv"))

  } else {
    cat("Use el parametro 'agregar' y haga un vector con las variables que desea agregar. \n O arregle las llaves del cuestionario")
  }
}
