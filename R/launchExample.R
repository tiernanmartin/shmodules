#' @title A launcher for example shiny apps
#'
#' @description \code{launchExample} provides an easy way to run examples
#'   of the modules supported in this package. These examples would ideally be
#'   located in the 'Help' documentation, but are too lengthy because they are
#'   shiny apps.
#' @param name A character string of the module (e.g. 'linkedScatter')
#' @param ... Any extra arguments.
#' @import shiny
#' @import devtools
#' @export


launchExample <- function(name, ...)
{
        dir <- paste('examples',name,sep = '/')
        shiny::runApp(appDir = system.file(dir, package = 'shmodules'),
                      ...)
}
