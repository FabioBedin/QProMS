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
    tags$div(
      class="container-xxl",
      ##spheres
      sphere(size = 5, top = 50, left = 40, z_index = 0),
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 2, top = 10, left = 75, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end"
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="pt-4 m-0", "Data wrangling")
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
              tags$h5(class="m-0 py-3 px-4 text-primary", "Valid values"),
              tags$div(
                class="px-4",
                shiny::sliderInput(
                  inputId = ns("slider_valid_val_thr"),
                  label = NULL,
                  min = 0,
                  max = 1,
                  value = 0.75,
                  step = 0.05
                ),
                shiny::selectInput(
                  inputId = ns("valid_val_type"),
                  label = NULL,
                  choices = c("alog", "each_grp", "total"),
                  selected = "alog"
                )
              ),
              tags$h5(class="m-0 px-4 pt-1 pb-3 text-primary", "Peptides filter"),
              tags$div(
                class="px-4",
                shiny::sliderInput(
                  inputId = ns("slider_peptide_thr"),
                  label = NULL,
                  min = 0,
                  max = 10,
                  value = 2
                ),
                shiny::selectInput(
                  inputId = ns("peptides_type"),
                  label = NULL,
                  choices = c("peptides", "unique", "razor"),
                  selected = "peptides"
                )
              ),
              tags$h5(class="m-0 px-4 pt-1 pb-3 text-primary", "Categorical filter"),
              tags$div(
                class="d-flex flex-row px-4",
                shinyWidgets::prettySwitch(inputId = ns("rev"), label = "Rev", value = TRUE, fill = TRUE, status = "success"),
                shinyWidgets::prettySwitch(inputId = ns("cont"), label = "Cont", value = TRUE, fill = TRUE, status = "success"),
                shinyWidgets::prettySwitch(inputId = ns("oibs"), label = "By site", value = TRUE, fill = TRUE, status = "success")
              ),
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
                  title = "Protein per samples",
                    tags$div(
                      class="p-4",
                      echarts4r::echarts4rOutput(ns("plot1"), height = "450")
                    )
                  ),
                tabPanel(
                  title = "Protein coverage",
                  tags$div(
                    class="p-4",
                    echarts4r::echarts4rOutput(ns("plot2"), height = "450")
                  )
                ),
                tabPanel(
                  title = "Missing data",
                  tags$div(
                    class="p-4",
                    echarts4r::echarts4rOutput(ns("plot3"), height = "450")
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

#' data_preprocessing Server Functions
#'
#' @noRd
mod_data_preprocessing_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$render, {

      if(r6$input_type == "MaxQuant"){

        r6$pg_wrangling(
          rev = input$rev,
          cont = input$cont,
          oibs = input$oibs,
          pep_col = input$peptides_type,
          pep_thr = input$slider_peptide_thr
          # rescue_cont = input$cont_vector
        )

        r6$filter_valid_val(
          data = r6$pg_filtered_data,
          type = input$valid_val_type,
          thr = input$slider_valid_val_thr
        )
      }else{
        r6$filter_valid_val(
          data = r6$data, ## da verificare questo, potrei aver bisogno di un data intermedio per generare il pivot long
          type = input$valid_val_type,
          thr = input$slider_valid_val_thr
        )
      }



      data <- r6$filtered_data
      expdes <- r6$expdesign

      r6$protein_counts_plot <- r6$protein_counts(data, expdes)

      r6$protein_coverage_plot <- r6$protein_coverage(data)

      r6$missing_data_plot <- r6$missing_data(data)
    })

    output$plot1 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$protein_counts_plot
    })

    output$plot2 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$protein_coverage_plot
    })

    output$plot3 <-  echarts4r::renderEcharts4r({

      shiny::req(input$render)

      r6$missing_data_plot
    })

    # observeEvent(input$guide, {
    #
    #   shiny::req(input$render)
    #
    #   rescue_cont <- data %>%
    #     dplyr::filter(potential_contaminant == "+") %>%
    #     dplyr::distinct(gene_names) %>%
    #     dplyr::pull()
    #
    #
    #   shinyWidgets::sendSweetAlert(
    #     session = session,
    #     title = "Tutorial",
    #     text = shinyWidgets::pickerInput(
    #       inputId = "cont_vector",
    #       label = "Select contaminant to rescue",
    #       choices = LETTERS,
    #       options = list(`live-search` = TRUE, `actions-box` = TRUE),
    #       multiple = TRUE
    #     ),
    #     type = NULL,
    #     html = TRUE
    #   )
    #
    # })

  })
}

## To be copied in the UI
# mod_data_preprocessing_ui("data_preprocessing_1")

## To be copied in the server
# mod_data_preprocessing_server("data_preprocessing_1")
