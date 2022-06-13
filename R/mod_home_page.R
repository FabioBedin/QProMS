#' home_page UI Function
#'
#' @description A shiny Module related to the home page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class="container",
      tags$div(
        class="glass-container"
      )
      # tags$div(
      #   class="shpere"
      # )
      # tags$nav(
      #
      # )
    )

  )
}

#' home_page Server Functions
#'
#' @noRd
mod_home_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_page_ui("home_page_1")

## To be copied in the server
# mod_home_page_server("home_page_1")
