#' normalization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_normalization_ui <- function(id){
  ns <- NS(id)
  tagList(
    data_wrangling_page(
      bg_sphere = sphere(size = 5, top = 70, left = 30, z_index = 0),
      title = "Normalization"
    )
  )
}

#' normalization Server Functions
#'
#' @noRd
mod_normalization_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_normalization_ui("normalization_1")

## To be copied in the server
# mod_normalization_server("normalization_1")
