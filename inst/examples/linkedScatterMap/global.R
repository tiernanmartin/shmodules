# library(sp)
library(rgdal)
library(plotly)
library(gplots)
library(ggplot2)
library(GISTools)
library(magrittr)
library(operator.tools)
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

checkboxInputStyle <- function (inputId, label, value = FALSE, width = NULL, cssStyle = NULL){
        value <- restoreInput(id = inputId, default = value)
        inputTag <- tags$input(id = inputId, type = "checkbox")
        if (!is.null(value) && value)
                inputTag$attribs$checked <- "checked"
        div(class = "form-group shiny-input-container",
            style = paste0(
                    ifelse(!is.null(width),paste0("width: ", validateCssUnit(width)),"")," ",
                    ifelse(!is.null(cssStyle),cssStyle,"")
            ),
            div(class = "checkbox",
                tags$label(inputTag, tags$span(label))))
}

columnStyle <- function(width, ..., offset = 0, cssStyle = NULL){
        if (!is.numeric(width) || (width < 1) || (width > 12))
                stop("column width must be between 1 and 12")
        colClass <- paste0("col-sm-", width)
        if (offset > 0)
                colClass <- paste0(colClass, " col-sm-offset-", offset)
        div(class = colClass,
            style = paste0(
                    ifelse(!is.null(cssStyle),
                           cssStyle,
                           "")),
            ...)
}

sidebarCSS <- function(){
                tags$style(HTML(
                        "
.dropdown-menu>.active>a, .dropdown-menu>.active>a:focus, .dropdown-menu>.active>a:hover {
                                            background-color: Transparent !important; border-color: Transparent !important;
                        font-weight: bold;
                        color: #FFFFFF; opacity: 1;
}
.dropdown-menu, .dropdown-menu>a:focus, .dropdown-menu>a:hover {
background-color: Transparent !important; border-color: Transparent !important;
}
table, th, td {
vertical-align: top !important;
}
.li {
list-style-type: disc;
list-style-position: inside;
text-indent: -1em;
padding-left: 1em;
}
.button {
-webkit-appearance: none; opacity: .5; color: #FFFFFF !important; background-color: Transparent !important; background-repeat:no-repeat; padding: 0px 0px 0px !important;border: none !important; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.button:hover {
opacity: 1; color: #FFFFFF !important; background-color: Transparent !important; background-repeat:no-repeat; padding: 0px 0px 0px !important;border: none !important; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.btn {
color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.btn-default {
color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.action-button {
color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.btn:hover{
color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.btn-default:hover{
color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.action-button:hover{
color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
}
.dataTables_wrapper .dataTables_info {
color: #FFFFFF; opacity: .75;
}
.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
color: #FFFFFF; opacity: .75;
}
.dataTables_wrapper .dataTables_paginate .paginate_button {
color: #FFFFFF !important;
}
table.dataTable tbody tr {
background-color: Transparent;
}
table.dataTable.no-footer {
border-bottom: 0px;
}
input, optgroup, select, textarea {
margin: 0;
font: inherit;
color: #222d32;
}
table.dataTable tbody .selected, table.dataTable .selected td.sorting_1, table.dataTable .selected td.sorting_2, table.dataTable .selected td.sorting_3, div.DTS tbody .even.selected, .table-striped tbody>.selected:nth-child(odd)>td, .table-striped tbody>.selected:nth-child(even)>td {
background-color: Transparent !important;
color: #3c8dbc;
}
table.dataTable tbody tr.even.active {
background-color: Transparent !important;
}
.pagination>li>a {
background: Transparent;
color: #FFFFFF; opacity: .75;
border-color: Transparent;
border-radius: 0!important;
}
.pagination>.active>a, .pagination>.active>a:focus, .pagination>.active>a:hover, .pagination>.active>span, .pagination>.active>span:focus, .pagination>.active>span:hover {
z-index: 2;
font-weight: bold;
color: #FFFFFF; opacity: 1;
cursor: default;
background-color: Transparent;
border-color: Transparent;
}

.pagination>.disabled>a, .pagination>.disabled>a:focus, .pagination>.disabled>a:hover, .pagination>.disabled>span, .pagination>.disabled>span:focus, .pagination>.disabled>span:hover{
color: #FFFFFF; opacity: .75;
cursor: default;
background-color: Transparent;
border-color: Transparent;
}
.multicol {
-webkit-column-count: 4; /* Chrome, Safari, Opera */
-moz-column-count: 4; /* Firefox */
column-count: 4;
-webkit-column-gap: 0px; /* Chrome, Safari, Opera */
-moz-column-gap: 0px; /* Firefox */                                      column-gap: 40px;
-webkit-column-width: 50px; /* Chrome, Safari, Opera */
column-width: 50px;
}
.control-label {
display: none;
margin-bottom: 0px; height: 0px;
}
.navbar-default,.navbar-default .navbar-nav>li>a {
color: #FFFFFF !important;
opacity: .75 !important;
background-color: Transparent !important;
border-color: color: #FFFFFF !important;
}
.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
color: #FFFFFF !important; opacity: 1 !important;
background-color: Transparent !important;
border-color: Transparent !important;
}
pre#orig_data_glimpse.shiny-text-output.shiny-bound-output {
color: #FFFFFF !important;
background-color: Transparent !important;
border: 0px;
}
.tab-content>.active {

}
.dataTables_scrollBody {
height: 450px !important; overflow-y: scroll; overflow-x: initial;
}
.form-control {
color: #FFFFFF !important; opacity: 1 !important;
background-color: #1e282c !important;
border-color: Transparent !important;
}
.selectize-input::after {
content: ' ';
display: block;
clear: left;

}
.selectize-input, .selectize-control.single .selectize-input.input-active {
background-color: Transparent !important;
border-color: Transparent !important;
}
.selectize-dropdown, .selectize-input, .selectize-input input {
color: #FFFFFF !important;
}
.selectize-control.single .selectize-input:after{
    content: ' ';
border-color: #fff transparent transparent transparent;
}
.selectize-control.single .selectize-input.dropdown-active:after {
    margin-top: -4px;
border-width: 0 5px 5px 5px;
border-color: transparent transparent #FFFFFF transparent !important;
}

.selectize-control.multi .selectize-input > div{
color: #FFFFFF !important; opacity: 1 !important;
background-color: #1e282c !important;
border-color: Transparent !important;
}
.selectize-input.focus {
box-shadow: none;
}
                        "
                        )
                        )
}

linkedScatterMapSidebarTabUI <- function(id,menu_item_name,tab_name) {
        ns <- NS(id)

        tagList(
                menuItem(text = menu_item_name,tabName = tab_name, icon = icon("globe"))
        )



}

linkedScatterMapSidebarTabContentUI <- function(id,menu_item_name,tab_name, sp) {
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
                                                                               column(width = 6),
                                                                               column(width = 6,
                                                                                      selectizeInput(inputId = ns('y_axis_linked'),label = 'Y:',choices = names(df)))
                                                              ),
                                                              conditionalPanel(condition = cond_linked_x_F,
                                                                               column(width = 6,selectizeInput(inputId = ns('x_axis'),label = 'X:',choices = names(df))),
                                                                               column(width = 6,
                                                                                      selectizeInput(inputId = ns('y_axis'),label = 'Y:',choices = names(df))))
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
                        geom_point(aes(x = sp_rx_id()[[x_axis()]],y = sp_rx_id()[[y_axis_control()]], key = KEY),color = proj_orange,alpha = .75) +
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
