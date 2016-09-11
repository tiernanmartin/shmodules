library(shiny)
library(leaflet)
library(RColorBrewer)

quakeMapUI <- function(id){
        ns <- NS(id)

        tagList(
                bootstrapPage(
                        fluidRow(
                                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                leafletOutput(ns("map"), width = "100%", height = "100%"),
                        ),


                absolutePanel(top = 10, right = 10,
                              sliderInput(ns("range"), "Magnitudes", min(quakes$mag), max(quakes$mag),
                                          value = range(quakes$mag), step = 0.1
                              ),
                              selectInput(ns("colors"), "Color Scheme",
                                          rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                              )
                )
                )

        )

}

quakeMap <- function(input, output, session, data_rx, id_copy){
        ns <- NS(id_copy)
        filteredData <- reactive({
                data_rx()[data_rx()$mag >= input$range[1] & data_rx()$mag <= input$range[2],]
        })

        colorpal <- reactive({
                colorNumeric(input$colors, data_rx()$mag)
        })

        output$map <- renderLeaflet({

                pal <- colorNumeric('Spectral',quakes$mag)

                leaflet(quakes) %>%
                        addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
                        addCircles(radius = ~10^mag/10,
                                   weight = 1, color = "#777777",
                                   fillColor = ~pal(mag), fillOpacity = 0.7,
                                   popup = ~paste(mag)
                                                   ) %>%
                        addLegend(position = "bottomright",
                                  pal = pal, values = ~mag
                        )
        })

        observe({
                pal <- colorpal()

                leafletProxy(session$ns("map"), data = filteredData()) %>%
                        clearShapes() %>%
                        clearControls() %>%
                        addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                                   fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                        ) %>%
                        addLegend(position = "bottomright",
                                  pal = pal, values = ~mag
                        )
        })

        # Un-comment the observe function below and the app starts working

        # observe({
        #         pal <- colorpal()
        #
        #         leafletProxy(ns("map"), data = filteredData()) %>%
        #                 clearShapes() %>%
        #                 clearControls() %>%
        #                 addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
        #                            fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
        #                 ) %>%
        #                 addLegend(position = "bottomright",
        #                           pal = pal, values = ~mag
        #                 )
        # })

}


ui <- quakeMapUI('test')

server <- function(input, output, session) {
        callModule(module = quakeMap,
                   id = 'test',
                   data_rx = reactive({quakes}),
                   id_copy = 'test')
}

shinyApp(ui, server)
