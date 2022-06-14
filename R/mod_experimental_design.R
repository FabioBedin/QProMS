#' experimental_design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_experimental_design_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' experimental_design Server Functions
#'
#' @noRd 
mod_experimental_design_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_experimental_design_ui("experimental_design_1")
    
## To be copied in the server
# mod_experimental_design_server("experimental_design_1")
