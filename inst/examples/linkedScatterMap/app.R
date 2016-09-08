# SETUP -----
# library(shiny)
# library(shinydashboard)
# library(DT)
# library(ggplot2)
# library(dplyr)
# library(shmodules) #devtools::install_github('tiernanmartin/shmodules')
source('global.R')

# UI -----

header <- dashboardHeader(title = "Map Template")

sidebar <- dashboardSidebar(
        width = "350px",
        sidebarMenu(id = 'menu',
                    linkedScatterMapSidebarTabUI('scatmap','Map with scatter plot','first')
        ),
        hr(),
        HTML("<div style='padding: 25px;'>"),
        linkedScatterMapSidebarTabContentUI('scatmap','Map with scatter plot','first',tracts),
        HTML("</div>")
)


body <- fluidDashboardBody(
        sidebarCollapsed = T,
        linkedScatterMapBodyUI(id = 'scatmap',tab_name = 'first')

)

ui <- dashboardPage(header,sidebar,body)

# SERVER  ------

server <- function(input, output, session) {

        callModule(module = linkedScatterMap,
                   id = "scatmap",
                   sp_rx = reactive({tracts}),
                   id2 = "scatmap"
        )
}

# RUN -----

shinyApp(ui, server)
