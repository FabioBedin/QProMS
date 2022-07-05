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
      class="container-xxl",
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
              inputId = ns("save_expdesign"),
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
              inputId = ns("guide"),
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
          # tags$div(
          #   class="col-12",
          #   glass_card(height = "55vh", DT::dataTableOutput(ns("expdesign_table")))
          # )
          tags$div(
            class="col-12",
            glass_card(height = "55vh", rhandsontable::rHandsontableOutput(ns("expdesign_table")))
          )
        )
      )
    )

  )
}

#' experimental_design Server Functions
#'
#' @noRd
mod_experimental_design_server <- function(id, r6, globalSession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$expdesign_table = DT::renderDataTable({
    #   # req(input$save_expdesign)
    #   gargoyle::watch("make_expdesign")
    #   r6$expdesign
    #   }, selection = 'none', rownames = FALSE, editable = 'all', options = list(pageLength = nrow(r6$expdesign)))
    #
    # proxy = DT::dataTableProxy('expdesign_table', session = globalSession)
    #
    # observeEvent(input$expdesign_table_cell_edit, {
    #
    #   r6$expdesign <<- DT::editData(data = r6$expdesign,
    #                                 info = input$expdesign_table_cell_edit,
    #                                 rownames = FALSE,
    #                                 proxy = proxy)
    # })
    #
    # observeEvent(input$save_expdesign, {
    #
    #   expdes <- r6$expdesign
    #
    #   # fare prima dei controlli su exp design
    #
    #   r6$standardize_pg_data(expdes)
    # })

    observeEvent(input$save_expdesign, {

      req(input$expdesign_table)

      if(!is.null(input$expdesign_table)){

        r6$expdesign <- rhandsontable::hot_to_r(input$expdesign_table)

        expdes <- r6$expdesign

        # fare prima dei controlli su exp design

        r6$standardize_pg_data(expdes)

      }
    })

    output$expdesign_table <- rhandsontable::renderRHandsontable({
      gargoyle::watch("make_expdesign")

      rhandsontable::rhandsontable(data = r6$expdesign)
    })

    observeEvent(input$guide, {

      expdes <- r6$expdesign

      # fare prima dei controlli su exp design

      print(expdes)
    })

  })
}

## To be copied in the UI
# mod_experimental_design_ui("experimental_design_1")

## To be copied in the server
# mod_experimental_design_server("experimental_design_1")
