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
      class="container",
      ##spheres
      sphere(size = 20, top = 50, left = 40, z_index = 0),
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 13, top = 10, left = 75, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end",
            shiny::actionButton(
              inputId = ns("render"),
              label = "Render Plots",
              class = "render-plot-btn"
            )
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="", "Data wrangling")
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
              icon("home", class = "home-icon fa-2x ms-3", lib = "font-awesome"),
              section = 1,
              slide = 0,
              outline = FALSE,
              clear = FALSE
            )
          )
        ),
        tags$div(
          class="row px-4 pt-4 justify-content-around",
          tags$div(
            class="col-4",
            glass_card(
              height = "150px",
              shiny::checkboxInput(inputId = ns("rev"), label = "Reverse", value = TRUE),
              shiny::checkboxInput(inputId = ns("cont"), label = "Contaminant", value = TRUE),
              shiny::checkboxInput(inputId = ns("oibs"), label = "Identify by site", value = TRUE)
            )
          ),
          tags$div(
            class="col-4",
            glass_card(
              height = "150px",
              shiny::selectInput(
                inputId = ns("peptides_type"),
                label = NULL,
                choices = c("peptides", "unique", "razor"),
                selected = "peptides"
              ),
              shiny::sliderInput(
                inputId = ns("slider_peptide_thr"),
                label = NULL,
                min = 0,
                max = 10,
                value = 2
              )
            )
          ),
          tags$div(
            class="col-4",
            glass_card(height = "150px")
          )
        ),
        tags$div(
          class="row p-4",
          tags$div(
            class="col-6",
            glass_card(height = "40vh", echarts4r::echarts4rOutput(ns("plot1")))
          ),
          tags$div(
            class="col-6",
            glass_card(height = "40vh")
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
      shiny::req(input$rev)
      shiny::req(input$cont)
      shiny::req(input$oibs)
      shiny::req(input$peptides_type)
      shiny::req(input$slider_peptide_thr)

      r6$pg_wrangling(rev = input$rev,
                      cont = input$cont,
                      oibs = input$oibs,
                      pep_col = input$peptides_type,
                      pep_thr = input$slider_peptide_thr)

      gargoyle::trigger("test_plot")
    })

    output$plot1 <-  echarts4r::renderEcharts4r({
      gargoyle::watch("test_plot")

      data <- r6$pg_filtered_data

      data %>%
        dplyr::group_by(label) %>%
        dplyr::summarise(counts = sum(bin_intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(., expdesign, by = "label") %>%
        dplyr::mutate(replicate = as.factor(replicate)) %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_charts(replicate) %>%
        # echarts4r::e_title(text = "Protein per sample", left = "center") %>%
        echarts4r::e_bar(counts) %>%
        echarts4r::e_tooltip(trigger = "item")
    })

  })
}

## To be copied in the UI
# mod_data_preprocessing_ui("data_preprocessing_1")

## To be copied in the server
# mod_data_preprocessing_server("data_preprocessing_1")
