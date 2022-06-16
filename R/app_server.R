#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_home_page_server("home_page_1")
  mod_experimental_design_server("experimental_design_1")
  mod_data_preprocessing_server("data_preprocessing_1")
  mod_normalization_server("normalization_1")
  mod_imputation_server("imputation_1")
}
