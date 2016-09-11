# library(sp)
library(rgdal)
library(plotly)
library(gplots)
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
blocks %<>% spTransform(crs_proj)

tracts@data %<>% mutate_at(vars(contains('P_')), funs(round_any(. * .01,.0001)))
blocks@data %<>% mutate_at(vars(contains('P_')), funs(round_any(. * .01,.0001)))

proj_light_grey <- col2hex("grey75")
proj_grey <- col2hex("grey50")
proj_dark_grey <- col2hex("grey25")
proj_orange <- '#D59C40'

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
                                          plotlyOutput(ns('scatter'), width = "auto")),
                                 fluidRow(width = 12,
                                          column(width = 6,
                                                 selectizeInput(inputId = ns('x_axis'),label = 'X Axis',choices = names(df))),
                                          column(width = 6,
                                                 selectizeInput(inputId = ns('y_axis'),label = 'Y Axis',choices = names(df)))
                                          )

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

linkedScatterMap <- function(input, output, session, sp_rx, plotly_event_rx) {

        ns <- session$ns

        # Reactives

        sp_rx_id <- reactive({
                sp_id <- sp_rx()
                sp_id@data <- cbind(sp_id@data,KEY = rownames(sp_id@data))
                return(sp_id)
        })

        var <- reactive({input$var})

        colorpal <- reactive({
                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]

                if(is.numeric(sp_rx()[[var()]])){
                        colorNumeric(myYlOrRd,sp_rx()[[var()]])
                }
                else{
                        colorFactor('Set1',sp_rx()[[var()]] %>% as.character() %>% factor)
                }

                })

        x_axis <- reactive({input$x_axis %>% as.character() %>% toupper()})

        y_axis <- reactive({input$y_axis %>% as.character() %>% toupper()})


        # Map
        output$map <- renderLeaflet({

                var1 <- names(sp_rx_id())[[1]]
                pal <- colorNumeric('Spectral',sp_rx_id()[[var1]])
                myLflt() %>%
                        addPolygons(data = sp_rx_id(),
                                    color = col2hex("white"),
                                    opacity = 1,
                                    weight = .5,
                                    fillColor = pal(sp_rx_id()[[var1]]),
                                    fillOpacity = .85,
                                    smoothFactor = 0,
                                    group = 'main') %>%
                        addLegend(position = "bottomleft",
                                  pal = pal, values = sp_rx_id()[[var1]])

        })

        # Map: change the fillColor and legend
        observe({
                pal <- colorpal()

                if(is.null(sub())){
                        leafletProxy(ns('map')) %>%
                                # clearShapes() %>%
                                clearShapes() %>%
                                clearControls() %>%
                                addPolygons(data = sp_rx_id(),
                                            color = col2hex("white"),
                                            opacity = 1,
                                            weight = .5,
                                            fillColor = pal(sp_rx_id()[[var()]]),
                                            fillOpacity = .85,
                                            smoothFactor = 0) %>%
                                addLegend(position = "bottomleft",
                                          pal = pal, values = sp_rx_id()[[var()]]) %>%
                                removeLayersControl()
                } else{
                        leafletProxy(ns('map')) %>%
                                clearShapes() %>%
                                clearControls() %>%
                                addPolygons(data = sp_rx_id(),
                                            color = col2hex("white"),
                                            opacity = 1,
                                            weight = .5,
                                            fillColor = pal(sp_rx_id()[[var()]]),
                                            fillOpacity = .85,
                                            smoothFactor = 0) %>%
                                addLegend(position = "bottomleft",
                                          pal = pal, values = sp_rx_id()[[var()]]) %>%
                                clearGroup(group = 'sub') %>%
                                addPolygons(data = sub(), fillOpacity = 0, color = '#00FFFF',
                                            opacity = 1, group = 'sub') %>%
                                addLayersControl(baseGroups = 'main',overlayGroups = 'sub',options = layersControlOptions(collapsed = TRUE)) %>%
                                removeLayersControl()
                }


        })

        # Scatter Plot
        output$scatter <- renderPlotly({

                # key <- sp_rx()$ARCINFOFPS # This will uniquely identify sp for Plotly

                # gg_df <- cbind(sp_rx()@data,KEY = rownames(sp_rx()@data))



                gg <- ggplot(sp_rx_id()@data) +
                        geom_point(aes(x = sp_rx_id()[[x_axis()]],y = sp_rx_id()[[y_axis()]], key = KEY),color = proj_orange,alpha = .75) +
                        xlab(x_axis()) + ylab(y_axis()) +
                        # scale_y_continuous(labels = scales::percent) +
                        # scale_x_continuous(labels = scales::percent) +
                        theme(plot.background = element_rect(fill = "transparent"),
                              panel.background = element_rect(fill = "transparent"),
                              text = element_text(color = "white"),
                              axis.text =  element_text(color = proj_grey),
                              axis.ticks = element_blank(),
                              panel.grid.major = element_line(color = proj_grey),
                              panel.grid.minor = element_line(color = proj_grey, size = 2),
                              axis.line.x = element_line(color = "white"),
                              axis.line.y = element_line(color = "white"))

                g <- ggplotly(gg, source = 'source') %>%
                        layout(dragmode = 'select',
                               margin = list(
                                       l = 60,
                                       r = 50,
                                       b = 50,
                                       t = 50
                               ),
                               font = list(family = 'Open Sans', size = 16)) %>%
                        config(displaylogo = FALSE,displayModeBar = FALSE)

                # Need to manually set the hoverinfo to avoid the key appearing in it
                build <- plotly_build(g)

                build$data[[1]]$text <- paste0(x_axis(),': ', as.character(sp_rx_id()[[x_axis()]]),'<br>',
                                               y_axis(),': ', as.character(sp_rx_id()[[y_axis()]]),'<br>'
                                               )

                build

        })

        # Graph-Map Interaction

        sub <- reactive({

                if (is.null(plotly_event_rx())) {

                        return(NULL) # do nothing

                } else {

                        sp_sel <- plotly_event_rx()[['key']]

                        if (length(sp_sel) == 0) {

                                sp_sel <- 'abcdefg' # a hack but it's working - set to something that can't be selected

                        }

                        if (!(sp_sel %in% sp_rx_id()[['KEY']])) {

                                return(NULL) # if there is not a match, do nothing as well

                        } else {

                                # Give back a sp data frame of the selected sp_rx_id
                                sub <- sp_rx_id()[which(sp_rx_id()[['KEY']] %in% sp_sel), ]

                                return(sub)

                        }

                }

        })


        # observe({
        #
        #         shiny::req(sub()) # Do this if sub() is not null
        #         shiny::req(var())
        #
        #         leafletProxy(ns('map')) %>%
        #                 clearGroup(group = 'sub') %>%
        #                 addPolygons(data = sub(), fill = FALSE, color = '#00FFFF',
        #                             opacity = 1, group = 'sub') %>%
        #                 addLayersControl(baseGroups = 'main',overlayGroups = 'sub',options = layersControlOptions(collapsed = TRUE))
        #                 # fitBounds(lng1 = bbox(sub())[1],
        #                 #           lat1 = bbox(sub())[2],
        #                 #           lng2 = bbox(sub())[3],
        #                 #           lat2 = bbox(sub())[4])
        #
        # })



        }
