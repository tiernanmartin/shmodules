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

#' @rdname shinyCSSStyle
#' @export

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
