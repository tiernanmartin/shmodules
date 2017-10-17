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

        args <- list("padding-top:" = paste0(pad_top,"px"),
                     "padding-right:" = paste0(pad_right,"px"),
                     "padding-bottom:" = paste0(pad_bottom,"px"),
                     "padding-left:" = paste0(pad_left,"px"))

        pad_string <- paste0(paste0(paste(lapply(names(args),c),lapply(args,c)),collapse = "; "),";")

  div(style = pad_string, ... )
}
