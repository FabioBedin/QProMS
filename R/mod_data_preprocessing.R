#' data_preprocessing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_preprocessing_ui <- function(id){
  ns <- NS(id)
  tagList(
    data_wrangling_page(
      bg_sphere = sphere(size = 20, top = 50, left = 40, z_index = 0),
      title = "Data pre-processing"
      )
  )
}

#' data_preprocessing Server Functions
#'
#' @noRd
mod_data_preprocessing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_preprocessing_ui("data_preprocessing_1")

## To be copied in the server
# mod_data_preprocessing_server("data_preprocessing_1")
