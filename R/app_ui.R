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
    showActiveTooltip = TRUE,
    loopBottom = FALSE,
    loopTop = FALSE,
    loopHorizontal = FALSE,
    scrollHorizontally = FALSE,
    resetSliders = FALSE,
    navigation = TRUE,
    navigationPosition = "left",
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
          menu = "home",
          class = "pt-0 unique-bg",
          mod_home_page_ui("home_page_1")
        ),
        fullPage::fullSection(
          menu = "uploading",
          class = "pt-0 unique-bg",
          mod_uploading_data_ui("uploading_data_1")
        ),
        fullPage::fullSection(
          menu = "expdesig",
          class = "pt-0 unique-bg",
          mod_experimental_design_ui("experimental_design_1")
        ),
        fullPage::fullSection(
          menu = "preprocessing",
          class = "pt-0 unique-bg",
          mod_data_preprocessing_ui("data_preprocessing_1")
        ),
        fullPage::fullSection(
          menu = "normalization",
          class = "pt-0 unique-bg",
          mod_normalization_ui("normalization_1")
        ),
        fullPage::fullSection(
          menu = "imputation",
          class = "pt-0 unique-bg",
          mod_imputation_ui("imputation_1")
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
