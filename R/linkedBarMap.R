#' @title A map with an interactive scatter plot
#'
#' @description \code{linkedScatterMap} provides a set of functions to build a
#' linked scatter plot-map module. It is currently set up for use in a
#' \code{\link{shinydashboard}} framework.
#'
#' @param id The module's unique identifier (to be passed to \code{\link[shiny]{NS()}})
#' @param menu_item_name Text to show for the menu item.
#' @param tab_name The name of a tab that this menu item will activate.
#' @param data A dataframe (or an object that can be correctly converted
#'  to a dataframe by \code{as.data.frame()}) to be visualized as a scatterplot.
#' @param cols Variables ("columns") from the dataframe to be visualized in the scatter plot (the select input boxes
#'  map to these parameters).
#' @return Functions ending in 'UI' return a string of HTML. The output types of non-UI functions varies.
#' @import htmltools
#' @import shiny
#' @import ggplot2
#' @export
#' @name linkedScatterMap

NULL

#' @rdname linkedScatterMap
#' @export
linkedBarMapSidebarTabUI <- function(id,menu_item_name,tab_name) {
        ns <- NS(id)

        menuItem(text = menu_item_name,tabName = tab_name, icon = icon("globe"))


}

#' @rdname linkedScatterMap
#' @export
linkedBarMapSidebarTabContentUI <- function(id,menu_item_name,tab_name, sp) {
        ns <- NS(id)
        # df <- sp %>% .@data %>% as.data.frame()
        df <- as.data.frame(sp@data)
        shiny::req(df)

        cond_tab <- paste0("input.menu == '",tab_name,"'") # the 'menu' part of 'input.menu' references the 'id' argument of `sidebarMenu` in app.R
        cond_linked_x_F <- sprintf("input['%s'] == false", ns("linked_x"))
        cond_linked_x_T <- sprintf("input['%s'] == true", ns("linked_x"))

        tagList(
                conditionalPanel(condition = cond_tab,
                                 navbarPage("",
                                            tabPanel(title = "Explore",
                                                     fluidRow(width = 12,
                                                              columnStyle(
                                                                      width = 9,
                                                                      selectizeInput(inputId = ns('var'),
                                                                                     label = 'Select a variable',
                                                                                     choices = names(df)
                                                                      )),
                                                              columnStyle(
                                                                      width = 3,
                                                                      checkboxInputStyle(inputId = ns('linked_x'), label = 'Set x-axis', value = TRUE,cssStyle = "padding: 0px;"),
                                                                      cssStyle = 'padding: 0px;')
                                                     ),
                                                     fluidRow(width = 12,
                                                              plotlyOutput(ns('scatter'), width = "auto")),
                                                     fluidRow(width = 12,
                                                              conditionalPanel(condition = cond_linked_x_T,
                                                                               fluidRow(
                                                                                       # column(width = 6),
                                                                                       column(width = 12,
                                                                                              selectizeInput(inputId = ns('y_axis_linked'),label = 'Y:',choices = names(df)))
                                                                               )

                                                              ),
                                                              conditionalPanel(condition = cond_linked_x_F,
                                                                               fluidRow(
                                                                                       column(width = 12,selectizeInput(inputId = ns('x_axis'),label = 'X:',choices = names(df)))
                                                                               ),
                                                                               fluidRow(
                                                                                       column(width = 12,
                                                                                              selectizeInput(inputId = ns('y_axis'),label = 'Y:',choices = names(df)))
                                                                               )
                                                              )

                                                     )
                                            ),
                                            tabPanel(title = "Style",
                                                     fluidRow(width = 12,
                                                              columnStyle(
                                                                      width = 9,
                                                                      selectizeInput(inputId = ns('pal'),
                                                                                     label = 'Select a color palette',
                                                                                     choices = c('Sequential',
                                                                                                 'Divergent',
                                                                                                 'Qualitative')
                                                                      )),
                                                              columnStyle(
                                                                      width = 3,
                                                                      checkboxInputStyle(inputId = ns('rev'), label = 'Reverse', value = FALSE,cssStyle = "padding: 0px;"),
                                                                      cssStyle = 'padding: 0px;')
                                                     )
                                            )
                                 )


                )
        )



}

#' @rdname linkedScatterMap
#' @export
linkedBarMapBodyUI <- function(id,tab_name) {
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

#' @rdname linkedScatterMap
#' @export
linkedBarMap <- function(input, output, session, sp_rx, plotly_event_rx) {

        ns <- session$ns
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]

        # Reactives

        sp_rx_id <- reactive({
                sp_id <- sp_rx()
                sp_id@data <- cbind(KEY = rownames(sp_id@data),sp_id@data)
                return(sp_id)
        })

        var <- reactive({input$var})

        pal_choice <- reactive({
                if(input$pal == 'Sequential'){
                        pal_c <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                        # if(input$rev){rev(pal_c)

                }else if(input$pal == 'Divergent'){
                        pal_c <- RColorBrewer::brewer.pal(n = 10,name = 'Spectral')[2:9]
                        # if(input$rev){rev(pal_c)

                }else{
                        pal_c <- RColorBrewer::brewer.pal(n = 9,name = 'Set1')
                        # if(input$rev){rev(pal_c)}
                }
        })


        pal_choice_rev <- reactive({
                if(input$rev){
                        rev(pal_choice())
                }else pal_choice()
        })


        colorpal <- reactive({

                if(is.numeric(sp_rx()[[var()]])){
                        colorNumeric(pal_choice_rev(),sp_rx()[[var()]])
                }
                else{
                        colorFactor(pal_choice_rev(),sp_rx()[[var()]] %>% as.character() %>% factor)
                }


                # if(is.numeric(sp_rx()[[var()]])){
                #         colorNumeric(myYlOrRd,sp_rx()[[var()]])
                # }
                # else{
                #         colorFactor('Spectral',sp_rx()[[var()]] %>% as.character() %>% factor)
                # }

        })

        x_axis <- reactive({

                if(linked_x()){
                        var() %>%as.character() %>% toupper()
                }else{
                        input$x_axis %>% as.character() %>% toupper()
                }
        })

        y_axis_linked <- reactive({input$y_axis_linked %>% as.character() %>% toupper()})

        y_axis <- reactive({
                input$y_axis %>% as.character() %>% toupper()
        })

        y_axis_control <- reactive({

                if(linked_x()){
                        input$y_axis_linked %>% as.character() %>% toupper()
                }else{
                        input$y_axis %>% as.character() %>% toupper()
                }

        })

        linked_x <- reactive({input$linked_x})



        # Rendered/Updated UI

        observeEvent(linked_x(),{
                if(!linked_x()){
                        updateSelectizeInput(session = session,
                                             inputId = 'y_axis',
                                             label = "Y:",
                                             choices = names(sp_rx()),
                                             selected = y_axis_linked())
                }
                else{
                        updateSelectizeInput(session = session,
                                             inputId = 'y_axis_linked',
                                             label = "Y:",
                                             choices = names(sp_rx()),
                                             selected = y_axis())
                }

        })



        # Map
        output$map <- renderLeaflet({

                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                var1 <- names(sp_rx_id())[[1]]

                # Check if the variable is numeric or something else,
                # assign a color ramp accordingly
                var1_type <- is.numeric(sp_rx_id()[[1]])
                pal <- function(x){
                        if(var1_type){colorNumeric(myYlOrRd,sp_rx_id()[[var1]])}
                        else{colorFactor('Spectral',sp_rx_id()[[var1]] %>% as.character() %>% factor)}
                }

                myLflt() %>%
                        addPolygons(data = sp_rx_id(),
                                    color = col2hex("white"),
                                    opacity = 1,
                                    weight = .5,
                                    fillColor = pal(sp_rx_id()[[var1]]),
                                    fillOpacity = .85,
                                    smoothFactor = 0,
                                    group = 'main') %>%
                        addLegend(position = "bottomleft", opacity = .85,
                                  pal = pal(), values = sp_rx_id()[[var1]])

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
                                addLegend(position = "bottomleft", opacity = .85,
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
                                addLegend(position = "bottomleft", opacity = .85,
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
                        geom_bar(aes(x = sp_rx_id()[[x_axis()]],y = sp_rx_id()[[y_axis_control()]], key = KEY),color = proj_orange,stat = 'identity', alpha = 1, fill = proj_orange) +
                        xlab(x_axis()) + ylab(y_axis_control()) +
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
                                               y_axis_control(),': ', as.character(sp_rx_id()[[y_axis_control()]]),'<br>'
                )

                build

        })

        # Graph Styling

        proj_light_grey <- col2hex("grey75")
        proj_grey <- col2hex("grey50")
        proj_dark_grey <- col2hex("grey25")
        proj_orange <- '#D59C40'

        # Graph-Map Interaction

        sub <- reactive({

                if (is.null(plotly_event_rx())) {

                        return(NULL) # do nothing

                } else {

                        sp_sel <- plotly_event_rx()[['key']]

                        if (length(sp_sel) == 0) {

                                sp_sel <- 'abcdefg' # a hack but it's working - set to something that can't be selected

                        }
                        ifelse(sp_sel %!in% sp_rx_id()[['KEY']],
                               return(NULL),
                               return(sp_rx_id()[which(sp_rx_id()[['KEY']] %in% sp_sel), ]))
                        # if (sp_sel %!in% sp_rx_id()[['KEY']]) {
                        #
                        #         return(NULL) # if there is not a match, do nothing as well
                        #
                        # } else {
                        #
                        #         # Give back a sp data frame of the selected sp_rx_id
                        #         sub <- sp_rx_id()[which(sp_rx_id()[['KEY']] %in% sp_sel), ]
                        #
                        #         return(sub)
                        #
                        # }

                }

        })




}
