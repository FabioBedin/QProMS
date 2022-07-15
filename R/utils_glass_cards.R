#' glass_cards
#'
#' @description A utils function for generate glass cards
#'
#' @return The return value are just simple html tags and custom css.
#'
#' @noRd
glass_card <- function(height, ...){
  tagList(
    tags$div(
      class = "glass-card",
      style = paste0("min-height: ", height, ";"),
      ...
    )
  )
}



#' sphere div
#'
#' @description A utils function for generate spheres
#'
#' @return The return value are just simple html tags and custom css.
#'
#' @noRd
sphere <- function(size, top, left, z_index){
  tagList(
    tags$div(
      class = "sphere",
      style = paste0("width: ", size, "rem; height: ", size, "rem; top: ", top, "%; left: ", left, "%; z-index: ", z_index, ";")
    )
  )
}


