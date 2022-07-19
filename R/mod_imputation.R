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
              tags$h6(class="m-0 px-4 py-1 text-center text-primary", "Down shift"),
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
                  title = "Missing data pie",
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

      r6$effect_of_imputation()


    })

    output$plot1 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$effect_of_imputation_plot
    })

    output$plot2 <- echarts4r::renderEcharts4r({

      shiny::req(input$render)

      icon_present <- "path://M10.219,1.688c-4.471,0-8.094,3.623-8.094,8.094s3.623,8.094,8.094,8.094s8.094-3.623,8.094-8.094S14.689,1.688,10.219,1.688M10.219,17.022c-3.994,0-7.242-3.247-7.242-7.241c0-3.994,3.248-7.242,7.242-7.242c3.994,0,7.241,3.248,7.241,7.242C17.46,13.775,14.213,17.022,10.219,17.022M15.099,7.03c-0.167-0.167-0.438-0.167-0.604,0.002L9.062,12.48l-2.269-2.277c-0.166-0.167-0.437-0.167-0.603,0c-0.166,0.166-0.168,0.437-0.002,0.603l2.573,2.578c0.079,0.08,0.188,0.125,0.3,0.125s0.222-0.045,0.303-0.125l5.736-5.751C15.268,7.466,15.265,7.196,15.099,7.03"

      icon_absent <- "path://M10.185,1.417c-4.741,0-8.583,3.842-8.583,8.583c0,4.74,3.842,8.582,8.583,8.582S18.768,14.74,18.768,10C18.768,5.259,14.926,1.417,10.185,1.417 M10.185,17.68c-4.235,0-7.679-3.445-7.679-7.68c0-4.235,3.444-7.679,7.679-7.679S17.864,5.765,17.864,10C17.864,14.234,14.42,17.68,10.185,17.68 M10.824,10l2.842-2.844c0.178-0.176,0.178-0.46,0-0.637c-0.177-0.178-0.461-0.178-0.637,0l-2.844,2.841L7.341,6.52c-0.176-0.178-0.46-0.178-0.637,0c-0.178,0.176-0.178,0.461,0,0.637L9.546,10l-2.841,2.844c-0.178,0.176-0.178,0.461,0,0.637c0.178,0.178,0.459,0.178,0.637,0l2.844-2.841l2.844,2.841c0.178,0.178,0.459,0.178,0.637,0c0.178-0.176,0.178-0.461,0-0.637L10.824,10z"

      pie_data <- r6$filtered_data

      dim <- nrow(pie_data)

      pie_data %>%
        dplyr::count(bin_intensity, name = "missing_data") %>%
        dplyr::mutate(missing_data = round(missing_data/dim*100,1)) %>%
        dplyr::mutate(label = paste0(missing_data, " %")) %>%
        echarts4r::e_charts(label) %>%
        echarts4r::e_pie(missing_data) %>%
        echarts4r::e_legend(icons = list(icon_absent, icon_present)) %>%
        echarts4r::e_title("Missing data persentage", "Defined across the entire dataset") %>%
        echarts4r::e_color(c("#440154", "#21918c")) %>%
        echarts4r::e_theme("QProMS_theme")
    })

  })
}

## To be copied in the UI
# mod_imputation_ui("imputation_1")

## To be copied in the server
# mod_imputation_server("imputation_1")
