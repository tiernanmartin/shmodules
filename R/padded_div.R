#' @title HTML Helper: add padding to div element.
#' @description Add padding to div element.
#' @name padded_div
#' @param ... html contents of div.
#' @param pad_top top padding (pixels)
#' @param pad_right right padding (pixels)
#' @param pad_bottom bottom padding (pixels)
#' @param pad_left left
#' @import shiny
#' @export
padded_div <- function(..., pad_top = 0, pad_right = 20, pad_bottom = 20, pad_left = 20){
  pad_string <- paste0("padding: ",paste(paste(c(pad_top,pad_right,pad_bottom, pad_left),"px",sep = ""),collapse = ";"))

  div(style = pad_string, ... )
}
