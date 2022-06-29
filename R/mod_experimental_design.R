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
          tags$div(
            class="col-12",
            glass_card(height = "55vh", DT::dataTableOutput(ns("expdesign_table")))
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
mod_experimental_design_server <- function(id, r6, globalSession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$expdesign_table = DT::renderDataTable({
      req(input$save_expdesign)
      r6$expdesign
      }, selection = 'none', rownames = FALSE, editable = 'all', options = list(pageLength = nrow(r6$expdesign)))

    proxy = DT::dataTableProxy('expdesign_table', session = globalSession)

    observeEvent(input$expdesign_table_cell_edit, {
      # info = input$expdesign_table_cell_edit
      # # str(info)
      # i = info$row
      # j = info$col
      # v = info$value
      # r6$expdesign[i, j] <<- DT::coerceValue(v, r6$expdesign[i, j])
      # DT::replaceData(proxy, r6$expdesign, resetPaging = FALSE, rownames = FALSE)

      r6$expdesign <<- DT::editData(data = r6$expdesign,
                                    info = input$expdesign_table_cell_edit,
                                    rownames = FALSE,
                                    proxy = proxy)
    })

    observeEvent(input$guide, {
      print(r6$expdesign)
    })

  })
}

## To be copied in the UI
# mod_experimental_design_ui("experimental_design_1")

## To be copied in the server
# mod_experimental_design_server("experimental_design_1")
