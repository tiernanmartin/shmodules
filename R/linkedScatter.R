#' @title A pair scatter plots with linked brushing
#'
#' @description \code{linkedScatter} provides a set of functions to build a
#' linked scatter plot module. It is currently set up for use in a
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
#' @name linkedScatter

NULL

#' @rdname linkedScatter
linkedScatterSidebarTabUI <- function(id,menu_item_name,tab_name) {
        ns <- NS(id)

        menuItem(text = menu_item_name,tabName = tab_name, icon = icon("bar-chart"))


}

#' @rdname linkedScatter
linkedScatterSidebarContentUI <- function(id,menu_item_name,tab_name, data) {
        ns <- NS(id)
        df <- as.data.frame(data)
        shiny::req(df)

        cond <- paste0("input.menu == '",tab_name,"'") # the 'menu' part of 'input.menu' references the 'id' argument of `sidebarMenu` in app.R
        cols <- df %>% select_if(is.numeric) %>% names # note: requires numeric variables (ideal for scatter plotting)
        tagList(
                conditionalPanel(condition = cond,
                                 htmltools::h4('Left Graph'),
                                 fluidRow(

                                         column(width = 6,
                                                selectInput(inputId = ns('left_x'),
                                                            label = 'X Axis',
                                                            choices = cols)),
                                         column(width = 6,
                                                selectInput(inputId = ns('left_y'),
                                                            label = 'Y Axis',
                                                            choices = cols))
                                 ),
                                 htmltools::h4('Right Graph'),
                                 fluidRow(

                                         column(width = 6,
                                                selectInput(inputId = ns('right_x'),
                                                            label = 'X Axis',
                                                            choices = cols)),
                                         column(width = 6,
                                                selectInput(inputId = ns('right_y'),
                                                            label = 'Y Axis',
                                                            choices = cols))
                                 )
                )
        )



}

#' @rdname linkedScatter
linkedScatterBodyUI <- function(id,tab_name) {
        ns <- NS(id)

        tabItem(tabName = tab_name,
                fluidRow(
                        column(width = 6,
                               box(width = '100%',
                                   plotOutput(ns("plotLeft"), brush = ns("brush"))
                               )
                        ),
                        column(width = 6,
                               box(width = '100%',
                                   plotOutput(ns("plotRight"), brush = ns("brush"))
                               )
                        )
                )

        )


}

#' @rdname linkedScatter
linkedScatter <- function(input, output, session, data) {

        left_x <- reactive({input$left_x})
        left_y <- reactive({input$left_y})
        right_x <- reactive({input$right_x})
        right_y <- reactive({input$right_y})

        # Yields the data frame with an additional column "selected_"
        # that indicates whether that observation is brushed
        dataWithSelection <- reactive({
                brushedPoints(data(), input$brush, allRows = TRUE)
        })

        output$plotLeft <- renderPlot({
                scatterPlot(dataWithSelection(), c(left_x(),left_y()))
        })

        output$plotRight <- renderPlot({
                scatterPlot(dataWithSelection(), c(right_x(),right_y()))
        })

        return(dataWithSelection)
}

#' @rdname linkedScatter
scatterPlot <- function(data, cols) {
        ggplot(data, aes_string(x = cols[1], y = cols[2])) +
                geom_point(aes(color = selected_)) +
                scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}
