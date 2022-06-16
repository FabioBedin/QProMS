#' imputation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    data_wrangling_page(
      bg_sphere = sphere(size = 7, top = 70, left = 50, z_index = 0),
      title = "Imputation"
    )
  )
}

#' imputation Server Functions
#'
#' @noRd
mod_imputation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_imputation_ui("imputation_1")

## To be copied in the server
# mod_imputation_server("imputation_1")
