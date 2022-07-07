#' uploading_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uploading_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class="container-xxl",
      ##spheres
      sphere(size = 5, top = 70, left = 30, z_index = 0),
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 13, top = 10, left = 75, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end"
            # fullButtonDownCustom("Down", id="custom_test")
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="", "Uploading data")
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
          class="row p-4",
          tags$div(
            class="col-3",
            glass_card(
              height = "550px",
              tags$h5(class="m-0 px-3 pt-4", "Browse:"),
              fileInputCustom(ns("load_from_file"), "", accept = ".txt"),
              tags$h5(class="m-0 p-3", "Intensity type:"),
              shiny::radioButtons(
                inputId = ns("radio_input"),
                label = NULL,
                inline = TRUE,
                choices = c("LFQ", "iBAQ", "Intenisty", "TMT"),
                selected = "LFQ"
              ),
              tags$h5(class="m-0 p-3", "Input type:"),
              shiny::radioButtons(
                inputId = ns("radio_input2"),
                label = NULL,
                inline = TRUE,
                choices = c("MaxQuant", "Other"),
                selected = "MaxQuant"
              ),
              tags$br(),
              div(
                class="d-flex justify-content-center px-3 py-5",
                shiny::actionButton(
                  inputId = ns("preview_upload_data"),
                  label = "Upload",
                  class = "render-plot-btn"
                )
              )
            )
          ),
          tags$div(
            class="col-9",
            glass_card(height = "550px", DT::DTOutput(ns("preview_table")))
          )
        )
      )
    )
  )
}

#' uploading_data Server Functions
#'
#' @noRd
mod_uploading_data_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$preview_upload_data, {
      req(input$load_from_file)
      req(input$radio_input2)
      req(input$radio_input)

      r6$loading_data(data_input = input$load_from_file, input_type = input$radio_input2, intensity_type = input$radio_input)

      if(input$radio_input2 == "MaxQuant"){
        r6$make_unique_names_pg()
      }

      r6$make_expdesign()

      gargoyle::trigger("make_expdesign")

    })

    output$preview_table = DT::renderDT({
      req(input$preview_upload_data)

      table <- r6$data

      table %>%
        dplyr::select(gene_names, starts_with("lfq_intensity")) %>%
        DT::datatable(options = list(
          columnDefs = list(list(className = 'dt-center', width = '200px', targets = '_all')),
          scrollX = 500,
          pageLength = 10))
    })

  })
}

## To be copied in the UI
# mod_uploading_data_ui("uploading_data_1")

## To be copied in the server
# mod_uploading_data_server("uploading_data_1")
