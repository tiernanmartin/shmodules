# SETUP -----
# library(shiny)
# library(shinydashboard)
# library(DT)
# library(ggplot2)
# library(dplyr)
# library(shmodules) #devtools::install_github('tiernanmartin/shmodules')
source('global.R')

# UI -----

header <- dashboardHeader(title = "Map Template",titleWidth = "350px")

sidebar <- dashboardSidebar(
        width = "350px",
        sidebarMenu(id = 'menu',
                    linkedScatterMapSidebarTabUI('scatmap','Map with scatter plot','first'),
                    linkedScatterMapSidebarTabUI('scatmap2','Map with scatter plot','second')
        ),
        HTML("<hr style='margin: 5px;height:1px;border-width:0;color:#404040;background-color:#404040'>"),
        HTML("<div style='padding-right: 25px;padding-left: 25px;'>"),
        linkedScatterMapSidebarTabContentUI('scatmap','Map with scatter plot','first',tracts),
        linkedScatterMapSidebarTabContentUI('scatmap2','Map with scatter plot','second',blocks),
        HTML("</div>")
)


body <- fluidDashboardBody(
        sidebarCollapsed = T,
        tabItems(
                linkedScatterMapBodyUI(id = 'scatmap',tab_name = 'first'),
                linkedScatterMapBodyUI(id = 'scatmap2',tab_name = 'second')
                )



)

ui <- dashboardPage(header,sidebar,body, skin = 'yellow')

# SERVER  ------

server <- function(input, output, session) {

        plotly_event <- reactive({event_data('plotly_selected', source = 'source')})

        callModule(module = linkedScatterMap,
                   id = "scatmap",
                   sp_rx = reactive({tracts}),
                   plotly_event_rx = reactive({plotly_event()})
        )
        callModule(module = linkedScatterMap,
                   id = "scatmap2",
                   sp_rx = reactive({blocks}),
                   plotly_event_rx = reactive({plotly_event()})
        )
}

# RUN -----

shinyApp(ui, server)
