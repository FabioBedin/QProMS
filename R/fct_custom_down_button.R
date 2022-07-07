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


shinyInputLabel <- function(inputId, label=NULL) {
  tags$label(label,
             class = "control-label",
             class = if (is.null(label)) "shiny-label-null",
             `for` = inputId
  )
}

#' custom_fileImput
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fileInputCustom <- function (inputId, label, multiple = FALSE, accept = NULL, width = NULL,
                             buttonLabel = "Browse...", placeholder = "No file selected")
{
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file",
                         style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
                         `data-restore` = restoredValue)
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(
    class = "form-group shiny-input-container px-3 m-0 w-100",
    style = htmltools::css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    div(
      class = "input-group",
      style = "border-radius: 30px; box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1); border-top: 1px solid rgba(230, 226, 221, 0.5); border-left: 1px solid rgba(230, 226, 221, 0.5); border-right: 1px solid rgba(230, 226, 221, 0.25); border-bottom: 1px solid rgba(230, 226, 221, 0.25); margin-bottom: .5rem;",
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(class = "btn btn-default btn-file custom-file-imput-1", buttonLabel,
             inputTag)
      ),
      tags$input(
        type = "text",
        class = "form-control",
        style = "border-radius: 0 30px 30px 0; background: rgba(230, 226, 221, 0.1);",
        placeholder = placeholder,
        readonly = "readonly"
      )
    ),
    tags$div(
      id = paste(inputId, "_progress", sep = ""),
      class = "progress active shiny-file-input-progress",
      tags$div(class = "progress-bar", style="border-radius: 20px")
    )
  )
}
