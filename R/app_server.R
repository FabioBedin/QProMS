#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  options(shiny.maxRequestSize=10000*1024^2)
  # Your application server logic

  gargoyle::init("make_expdesign", "test_plot") #, "test_plot"

  # greate a new QProMS object
  qproms_object <- QProMS$new()

  ## all mod
  mod_home_page_server("home_page_1")
  mod_uploading_data_server("uploading_data_1", r6 = qproms_object)
  mod_experimental_design_server("experimental_design_1", r6 = qproms_object, globalSession = session)
  mod_data_preprocessing_server("data_preprocessing_1", r6 = qproms_object)
  mod_normalization_server("normalization_1")
  mod_imputation_server("imputation_1")
}
