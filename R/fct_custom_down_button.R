#' custom_down_button
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fullButtonDownCustom <- function (..., id, outline = FALSE, clear = FALSE)
{
  button <- fullPage::fullButton(..., id = id, outline = outline, clear = clear)
  JS <- paste0("$(document).on('click', '#", id, "', function(){$.fn.fullpage.moveSectionDown();});")
  shiny::tagList(shiny::tags$head(shiny::tags$script(JS)),
                 button)
}
