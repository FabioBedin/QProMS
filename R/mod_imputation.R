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
    tags$div(
      class="container-xxl",
      ##spheres
      sphere(size = 7, top = 50, left = 50, z_index = 0),
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 1.1, top = 90, left = 55, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end"
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="pt-4 m-0", "Imputation")
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
              tags$h5(class="m-0 py-3 px-4 text-primary", "Strategies"),
              tags$div(
                class="px-4",
                shiny::selectInput(
                  inputId = ns("imputation_type"),
                  label = NULL,
                  choices = c("Mixed", "Perseus", "None"),
                  selected = "Mixed"
                )
              ),
              tags$h6(class="m-0 px-4 py-1 text-center text-primary", "Shift"),
              tags$div(
                class="px-4",
                shiny::sliderInput(
                  inputId = ns("shift_thr"),
                  label = NULL,
                  min = 1.2,
                  max = 2.4,
                  value = 1.8,
                  step = 0.1
                )
              ),
              tags$h6(class="m-0 px-4 py-1 text-center text-primary", "Scale"),
              tags$div(
                class="px-4",
                shiny::sliderInput(
                  inputId = ns("scale_thr"),
                  label = NULL,
                  min = 0.1,
                  max = 0.5,
                  value = 0.3,
                  step = 0.1
                )
              ),
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
              tabsetPanel(
                tabPanel(
                  title = "Imputed data",
                  tags$div(
                    class="p-4",
                    echarts4r::echarts4rOutput(ns("plot1"), height = "450")
                  )
                ),
                tabPanel(
                  title = "Missing data matrix",
                  tags$div(
                    class="p-4",
                    echarts4r::echarts4rOutput(ns("plot2"), height = "450")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' imputation Server Functions
#'
#' @noRd
mod_imputation_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$render, {

      req(input$imputation_type)
      req(input$shift_thr)
      req(input$scale_thr)

      if(input$imputation_type == "Mixed"){
        r6$is_mixed <- TRUE
        r6$is_imp <- TRUE
        r6$perseus_imputation(shift = input$shift_thr, scale = input$scale_thr)
      }else if(input$imputation_type == "Perseus"){
        r6$is_mixed <- FALSE
        r6$is_imp <- TRUE
        r6$perseus_imputation(shift = input$shift_thr, scale = input$scale_thr)
      }else{
        r6$is_mixed <- FALSE
        r6$is_imp <- FALSE
      }

      r6$effect_of_imputation_plot()


    })

    output$plot1 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$imputation_plot
    })

  })
}

## To be copied in the UI
# mod_imputation_ui("imputation_1")

## To be copied in the server
# mod_imputation_server("imputation_1")
