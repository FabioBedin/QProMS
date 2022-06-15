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
      sphere(size = 20, top = 50, left = 40, z_index = 0),
      sphere(size = 10, top = 80, left = 70, z_index = 0),
      sphere(size = 25, top = -15, left = 93, z_index = 0),
      tags$div(
        class="glass-container",
        sphere(size = 12, top = 10, left = -8, z_index = 4),
        sphere(size = 8, top = 16, left = 46, z_index = 1),
        tags$nav(
          class="container pt-5 px-5",
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
        tags$div(
          style="min-height: 5vh;"
        ),
        tags$div(
          class="container",
          tags$div(
            class="row p-4",
            tags$div(
              class="col-6",
              glass_card(
                height = "auto",
                HTML(
                  '<h1 class="px-5 mt-5 pt-4 display-5" style="color: #e6e2dd;"><span class="h1-color">Q</span>uantitative <span class="h1-color">Pro</span>teomics <span class="h1-color">M</span>ade <span class="h1-color">S</span>imple</h1>'
                ),
                tags$p(
                  class = "pt-2 px-5 lead",
                  style = "color: #e6e2dd;",
                  "Data analysis application suited for MaxQuant output."
                ),
                tags$div(
                  class = "px-5 py-2 my-5",
                  actionButton(
                    inputId = ns("start"),
                    label = "START",
                    class = "btn btn-lg rounded-pill shadow m-2 text-light fw-bold py-2 px-5 btn-gradient"
                  )
                )
              )
            ),
            tags$div(
              class="col-6",
              tags$div(
                style = "min-height: 60vh;"
              )
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
