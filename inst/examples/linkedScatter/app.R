# SETUP -----
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(shmodules) #devtools::install_github('tiernanmartin/shmodules')


# UI -----

header <- dashboardHeader(title = "Basic dashboard",
                          titleWidth = '350px')

sidebar <- dashboardSidebar(
        width = "350px",
        sidebarMenu(id = 'menu',
                linkedScatterSidebarTabUI('scatters','First Graph','first'),
                linkedScatterSidebarTabUI('scatters2','Second Graph','second')
        ),
        hr(),
        HTML("<div style='padding: 25px;'>"),
        linkedScatterSidebarContentUI('scatters','First Graph','first',mtcars),
        linkedScatterSidebarContentUI('scatters2','Second Graph','second',iris),
        HTML("</div>")
)

body <- fluidDashboardBody(
        # tags$script(
        #         '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
        # ),
        tabItems(
                linkedScatterBodyUI('scatters','first'),
                linkedScatterBodyUI('scatters2','second')
        )

)


ui <- dashboardPage(header,sidebar,body)

# SERVER  ------

server <- function(input, output, session) {

        callModule(linkedScatter, "scatters",
                   data = reactive(mtcars)
        )
        callModule(linkedScatter, "scatters2",
                   data = reactive(iris)
        )
}
# RUN -----

shinyApp(ui, server)

