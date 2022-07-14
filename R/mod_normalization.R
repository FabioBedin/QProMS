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
    tags$div(
      class="container-xxl",
      ##spheres
      sphere(size = 4, top = 80, left = 60, z_index = 0),
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 1, top = 20, left = 35, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end"
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="pt-4 m-0", "Normalization")
          ),
          tags$div(
            class="col-3 text-end align-self-end d-flex justify-content-end",
            shiny::actionButton(
              inputId = ns("guide"),
              label = NULL,
              icon = icon("info", class = "fa", lib = "font-awesome"),
              class = "guide-icon"
            ),
            fullPage::fullButtonTo(
              icon("home", class = "home-icon fa ms-2", lib = "font-awesome"),
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
            class="col-3",
            glass_card(
              height = "550px",
              tags$h5(class="m-0 py-3 px-4 text-primary", "Methods"),
              tags$div(
                class="px-4",
                shiny::selectInput(
                  inputId = ns("norm_methods"),
                  label = NULL,
                  choices = c("None" , "vsn"),
                  selected = "None"
                ),
              ),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              div(
                class="d-flex justify-content-center px-4 pt-4 pb-1",
                shiny::actionButton(
                  inputId = ns("render"),
                  label = "Render Plots",
                  class = "render-plot-btn w-100"
                )
              )
            )
          ),
          tags$div(
            class="col-9",
            glass_card(
              height = "550px",
              tags$div(
                class="p-4",
                echarts4r::echarts4rOutput(ns("plot1"), height = "500")
              )
            )
          )
        )
      )
    )

  )
}

#' normalization Server Functions
#'
#' @noRd
mod_normalization_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$render, {

      req(input$norm_methods)

      r6$normalization(
        norm_methods = input$norm_methods,
        run_once = r6$vsn_norm_run_once
        )

    })

    output$plot1 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$normalization_plot
    })

  })
}

## To be copied in the UI
# mod_normalization_ui("normalization_1")

## To be copied in the server
# mod_normalization_server("normalization_1")
