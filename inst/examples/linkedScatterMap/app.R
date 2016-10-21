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
        shmodules::sidebarCSS(),
        sidebarMenu(id = 'menu',
                    shmodules::linkedBarMapSidebarTabUI('barmap1','Map with bar plot','first'),
                    shmodules::linkedScatterMapSidebarTabUI('scatmap2','Map with scatter plot','second')
        ),
        HTML("<hr style='margin: 5px;height:1px;border-width:0;color:#404040;background-color:#404040'>"),
        HTML("<div style='padding-right: 25px;padding-left: 25px;'>"),
        shmodules::linkedBarMapSidebarTabContentUI('barmap1','Map with bar plot','first',tracts),
        shmodules::linkedScatterMapSidebarTabContentUI('scatmap2','Map with scatter plot','second',blocks),
        HTML("</div>")
)


body <- shmodules::fluidDashboardBody(sidebarCollapsed = FALSE,
        tabItems(
                shmodules::linkedBarMapBodyUI(id = 'barmap1',tab_name = 'first'),
                shmodules::linkedScatterMapBodyUI(id = 'scatmap2',tab_name = 'second')
                )



)

ui <- dashboardPage(header,sidebar,body, skin = 'yellow')

# SERVER  ------

server <- function(input, output, session) {

        plotly_event <- reactive({event_data('plotly_selected', source = 'source')})

        callModule(module = shmodules::linkedBarMap,
                   id = "barmap1",
                   sp_rx = reactive({tracts}),
                   plotly_event_rx = reactive({plotly_event()})
        )
        callModule(module = shmodules::linkedScatterMap,
                   id = "scatmap2",
                   sp_rx = reactive({blocks}),
                   plotly_event_rx = reactive({plotly_event()})
        )
}

# RUN -----

shinyApp(ui, server)
