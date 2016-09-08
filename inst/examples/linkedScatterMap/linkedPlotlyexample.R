library(plotly)

# http://stackoverflow.com/questions/37151795/plotly-heatmap-scatter-linked-in-shiny-not-working-in-a-module?rq=1
#### Define Modules ####
correlation_matrix_shinyUI <- function(id) {
        ns <- NS(id)

        mainPanel(
                plotlyOutput(ns("corr_matrix"), height = '650px'),
                plotlyOutput(ns("scatterplot"), height = '550px')
        )
}

correlation_matrix_shiny <- function(input, output, session, plotlyEvent) {

        data_df <- reactive({
                mtcars
        })

        corr_data <- reactive({
                if (is.null(data_df()))
                        return()

                corr_data <- cor(data_df())
                diag(corr_data) <- NA
                corr_data <- round(corr_data, 4)
                corr_data
        })

        corr_names <- reactive({
                if (is.null(data_df()))
                        return()

                corr_names <- colnames(data_df())
                corr_names
        })

        output$corr_matrix <- renderPlotly({
                if (is.null(corr_names()))
                        return()
                if (is.null(corr_data()))
                        return()


                g <- plot_ly(x = corr_names(), y = corr_names(), z = corr_data(),
                             key = corr_data(), type = "heatmap", source = "CORR_MATRIX", zmax = 1, zmin = -1)
                g
        })

        output$scatterplot <- renderPlotly({
                if (is.null(data_df()))
                        return()

                data_use <- data_df()

                s <- plotlyEvent()

                if (length(s)) {
                        vars <- c(s[["x"]], s[["y"]])
                        d <- setNames(data_use[vars], c("x", "y"))
                        yhat <- fitted(lm(y ~ x, data = d))
                        plot_ly(d, x = x, y = y, mode = "markers") %>%
                                plotly::add_trace(x = x, y = yhat, mode = "lines") %>%
                                plotly::layout(xaxis = list(title = s[["x"]]),
                                               yaxis = list(title = s[["y"]]),
                                               showlegend = FALSE)
                } else {
                        plot_ly()
                }
        })

}
############ End Module Definition ######

ui <- shinyUI(fluidPage(
        sidebarLayout(
                sidebarPanel(
                ),
                correlation_matrix_shinyUI(id = "cor_module")
        )
))

server <- function(input, output, session) {

        plotlyEvent <- reactive(event_data("plotly_click", source = "CORR_MATRIX"))

        callModule(correlation_matrix_shiny, id = "cor_module", reactive(plotlyEvent()))
}

shinyApp(ui = ui, server = server)
