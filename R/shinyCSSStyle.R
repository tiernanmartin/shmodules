#' @title Shiny UI function with 'style = ' arguments
#' @description \code{shinyCSSStyle} extend several common UI functions by adding a argument for css styling of a single element.
#' @param inputId The module's unique identifier (to be passed to \code{\link[shiny]{NS()}}).
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value (TRUE or FALSE).
#' @param width The width of the input, e.g. '400px', or '100\%'.
#' @param offset The number of columns to offset this column from the end of the previous column.
#' @param ... Elements to include within the UI structure column (e.g. column).
#' @param cssStyle A string of style arguments using CSS syntax (e.g., 'padding:0px; margin-top: 5px;).
#' @return The UI element (as HTML) with any specified CSS styling.
#' @import htmltools
#' @import shiny
#' @name shinyCSSStyle

NULL

#' @rdname shinyCSSStyle
#' @export
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

#' @rdname shinyCSSStyle
#' @export
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
