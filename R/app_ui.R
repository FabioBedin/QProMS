#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  options <- list(
    sectionsColor = c('#262222', '#262222'),
    scrollingSpeed = 1000,
    loopBottom = FALSE,
    loopTop = FALSE,
    loopHorizontal = FALSE,
    scrollHorizontally = FALSE,
    resetSliders = FALSE,
    navigationPosition = "none",
    responsiveSlides = TRUE,
    controlArrows = FALSE
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    shiny::bootstrapPage(
      theme = bslib::bs_theme(
        version = 5,
        base_font = bslib::font_google("Roboto"),
        primary = "#75BC56", #verde
        fg = "#e6e2dd", #grigio chiaro
        bg = "#262222" # grigio scuro
      ),

      fullPage::fullPage(
        opts = options,
        menu = NULL,
        fullPage::fullSection(
          class = "pt-0 unique-bg",
          mod_home_page_ui("home_page_1")
        ),
        fullPage::fullSection(
          class = "pt-0 unique-bg",
          mod_experimental_design_ui("experimental_design_1")
        )
      )

    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "QProMS"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
