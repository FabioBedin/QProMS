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
    tags$div(
      class="container",
      sphere(size = 18, top = 2, left = 20, z_index = 0),
      tags$div(
        class="glass-container",
        sphere(size = 13, top = 19, left = 78, z_index = 1),
        sphere(size = 8, top = 95, left = 30, z_index = 4),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end",
            shiny::actionButton(
              inputId = "save_expdesign",
              label = "save",
              class = "render-plot-btn"
            )
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="ps-4", "Experimental design")
          ),
          tags$div(
            class="col-3 text-end align-self-end d-flex justify-content-end",
            shiny::actionButton(
              inputId = "guide",
              label = NULL,
              icon = icon("info", class = "fa-2x", lib = "font-awesome"),
              class = "guide-icon"
            ),
            fullPage::fullButtonTo(
              icon("home", class = "home-icon fa-2x  ms-3", lib = "font-awesome"),
              section = 1,
              slide = 0,
              outline = FALSE,
              clear = FALSE
            )
          )
        ),
        tags$div(
          class="row p-4",
          tags$div(
            class="col-12",
            glass_card(height = "55vh")
          )
          # tags$div(
          #   class="col-3 align-self-center",
          #   glass_card(height = "150px")
          # )
        )
        # tags$h2(class="text-center", "ProteinGroups"),
        # tags$div(
        #   class="row p-4",
        #   tags$div(
        #     class="col-12",
        #     glass_card(height = "35vh")
        #   )
        # )
      )
    )

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
