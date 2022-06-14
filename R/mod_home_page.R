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
        class="sphere-1"
      ),
      tags$div(
        class="sphere-4"
      ),
      tags$div(
        class="sphere-5"
      ),
      tags$div(
        class="glass-container",
        tags$div(
          class="sphere-2"
        ),
        tags$div(
          class="sphere-3"
        ),
        tags$nav(
          class="container p-5",
          tags$header(
            tags$a(herf="#", class = "navbar-brand", "QProMS")
            # tags$ul(
            #   class="",
            #   tags$li(tags$a(herf="#", "Home")),
            #   tags$li(tags$a(herf="#", "About")),
            #   tags$li(tags$a(herf="#", "Contact"))
            # )
          )
        ),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$div(
          class="container",
          tags$div(
            class="row justify-content-evenly",
            tags$div(
              class="col-6",
              tags$div(
                class="glass-card",
                HTML('<h1 class="px-5 mt-5 pt-4 display-5" style="color: #e6e2dd;"><span class="h1-color">Q</span>uantitative <span class="h1-color">Pro</span>teomics <span class="h1-color">M</span>ade <span class="h1-color">S</span>imple</h1>'),
                tags$p(class="pt-2 px-5 lead", style="color: #e6e2dd;", "Data analysis application suited for MaxQuant output."),
                tags$div(
                  class="px-5 mt-5",
                  actionButton(
                    inputId = ns("start"),
                    label = "START",
                    class = "btn btn-lg rounded-pill shadow m-2 text-light fw-bold btn-gradient py-2 px-5"
                  )
                )
              )
            ),
            tags$div(
              class="col-5"
            )
          )
        )
      )
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
