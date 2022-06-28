#' qc_page
#'
#' @description A utils function for generate the HTML structure of all quality control pages
#'
#' @return The return value are just simple html tags and custom css.
#'
#' @noRd
data_wrangling_page <- function(bg_sphere, title){
  tagList(
    tags$div(
      class="container",
      ##spheres
      bg_sphere,
      tags$div(
        class="glass-container",
        ##spheres
        sphere(size = 13, top = 10, left = 75, z_index = 1),
        tags$div(
          class="row px-4",
          tags$div(
            class="col-3 text-start align-self-end",
            shiny::actionButton(
              inputId = "Id105",
              label = "Render Plots",
              class = "render-plot-btn"
            )
          ),
          tags$div(
            class="col-6 text-center",
            tags$h2(class="", title)
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
            glass_card(height = "150px")
          ),
          tags$div(
            class="col-4",
            glass_card(height = "150px")
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
            glass_card(height = "40vh")
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
