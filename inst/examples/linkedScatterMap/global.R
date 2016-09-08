# library(sp)
library(rgdal)
library(plotly)
library(ggplot2)
library(GISTools)
library(magrittr)
library(shiny)
library(plyr)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(shmodules)

data(newhaven)

crs_proj <- CRS("+init=epsg:4326")

proj4string(tracts) <- proj4string(blocks)

tracts %<>% spTransform(crs_proj)

tracts@data %<>% mutate_at(vars(contains('P_')), funs(round_any(. * .01,.0001)))


myLflt <- function(){
        leaflet() %>%
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                )
}

linkedScatterMapSidebarTabUI <- function(id,menu_item_name,tab_name) {
        ns <- NS(id)

        menuItem(text = menu_item_name,tabName = tab_name, icon = icon("globe"))


}

linkedScatterMapSidebarTabContentUI <- function(id,menu_item_name,tab_name, sp) {
        ns <- NS(id)
        # df <- sp %>% .@data %>% as.data.frame()
        df <- as.data.frame(sp@data)
        shiny::req(df)

        cond <- paste0("input.menu == '",tab_name,"'") # the 'menu' part of 'input.menu' references the 'id' argument of `sidebarMenu` in app.R
        cols <- df %>% select_if(is.numeric) %>% names # note: requires numeric variables (ideal for scatter plotting)
        tagList(
                conditionalPanel(condition = cond,
                                 fluidRow(width = 12,
                                          selectizeInput(inputId = ns('var'),label = 'Select a variable',
                                                         choices = names(df)
                                          )),
                                 fluidRow(width = 12,
                                          plotlyOutput(ns('scatter'), width = "auto"))

                )
        )



}

linkedScatterMapBodyUI <- function(id,tab_name) {
        ns <- NS(id)

        tabItem(tabName = tab_name,
                tags$head(
                        tags$style(
                                HTML('
                                     section.content{
                                     padding:0px;
                                     }
                                     .outer {
                                     height: calc(100vh - 50px);
                                     padding: 0px;
                                     margin: 0;
                                     }
                                     '))),
                tags$div(class = 'outer', leafletOutput(ns("map"),height = "100%", width = '100%')
                )

        )


}

linkedScatterMap <- function(input, output, session, id2, sp_rx) {

        ns <- NS(id2)

        # Reactives

        var <- reactive({input$var})

        colorpal <- reactive({colorNumeric('Spectral',sp_rx()[[var()]])})


        # Map
        output$map <- renderLeaflet({

                pal <- colorNumeric('Spectral',sp_rx()$P_VACANT)
                myLflt() %>%
                        addPolygons(data = sp_rx(),
                                    opacity = 0,
                                    fillColor = ~pal(P_VACANT),
                                    fillOpacity = .85,
                                    smoothFactor = 0) %>%
                        addLegend(position = "bottomleft",
                                  pal = pal, values = sp_rx()$P_VACANT)

        })

        # Map: change the fillColor and legend
        observe({
                pal <- colorpal()

                map_id <- ns('map')

                leafletProxy(map_id) %>%
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(data = sp_rx(),
                                    opacity = 0,
                                    fillColor = pal(sp_rx()[[var()]]),
                                    fillOpacity = .85,
                                    smoothFactor = 0) %>%
                        addLegend(position = "bottomleft",
                                  pal = pal, values = sp_rx()[[var()]])
        })

        # Scatter Plot
        output$scatter <- renderPlotly({

                key <- sp_rx()$ARCINFOFPS # This will uniquely identify sp for Plotly

                p1a <- ggplot(sp_rx()@data) +
                        geom_point(aes(x = P_VACANT,y = P_RENTROCC, key = key)) +
                        theme_minimal(base_size = 14) +
                        scale_y_continuous(labels = scales::percent) +
                        scale_x_continuous(labels = scales::percent) +
                        xlab('Vacancy') + ylab('Renter-Occupancy')

                g <- ggplotly(p1a, source = 'source') %>%
                        layout(dragmode = 'select',
                               margin = list(l = 100),
                               font = list(family = 'Open Sans', size = 16))

                # Need to manually set the hoverinfo to avoid the key appearing in it
                build <- plotly_build(g)

                build$data[[1]]$text <- paste0('Tract ID: ', as.character(sp_rx()$ARCINFOFPS), '<br>',
                                               'Vacancy Rate: ', as.character(sp_rx()$P_VACANT),'<br>',
                                               'Renter-Occupancy Rate: ', as.character(sp_rx()$P_RENTROCC))

                build

        })
}
