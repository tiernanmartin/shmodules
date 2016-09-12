#' @title A leaflet map with Mapbox provider tiles
#'
#' @description \code{myLflt} provides a colorful basemap that can be used as the first argument of a Leaflet pipe chain.
#'
#' @return A leaflet map with provider tiles from Mapbox already loaded
#' @import leaflet
#' @export
#' @name myLflt

myLflt <- function(){
        leaflet() %>%
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                )
}
