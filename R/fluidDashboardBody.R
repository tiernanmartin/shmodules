#' @title A dashboard body that automatically resizes when the sidebar is toggled
#'
#' @description \code{fluidDashboardBody} inserts a javascript snippet at the beginning of
#' the standard \code{\link{dashboardBody}} outuput, which tells the content of the dashboard
#' body to automatically adjust its sizing when the sidebar button is toggled.
#'
#' It also provides the option to begin the app with the sidebar collapsed (\code{sidebarCollapsed = TRUE}).
#'
#' @param sidebarCollapsed If \code{TRUE}, a javascript snippet will be inserted to collapse
#'   the sidebar when the app is launched.
#' @param ... Items to put in the dashboard body.
#' @import htmltools
#' @export


fluidDashboardBody <- function(sidebarCollapsed = FALSE,...){



        htmltools::div(class = "content-wrapper",
                       if(sidebarCollapsed){tags$script(HTML('$(function() {$("body").addClass("sidebar-collapse");'
                                                             ))},
                       htmltools::tags$section(class = "content",
                                               htmltools::tags$script(
                                                       '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
                                               ),...))

}
