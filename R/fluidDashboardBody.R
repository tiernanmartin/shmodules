#' @title A Dashboard Body that Automatically Resizes when the Sidebar is Toggled
#'
#' @description \code{fluidDashboardBody} inserts a javascript "trigger "at the beginning of
#' the standard \code{\link{dashboardBody}} outuput, which tells the content of the dashboard
#' body to automatically adjust its sizing when the sidebar button is toggled.
#'
#' @param ... Items to put in the dashboard body.
#' @import htmltools
#' @export


fluidDashboardBody <- function(...){
        {
                htmltools::div(class = "content-wrapper",
                               htmltools::tags$section(class = "content",
                                                       htmltools::tags$script(
                                         '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
                                 ),...))
        }
}
