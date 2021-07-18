#' Return spatial polygon of hexagonal features with a count of the number of unique routes with a stop inside the hex area's contained in the specified counties
#'
#' @param gtfsObj object from gtfsfunctions::formatGTFSObject()
#' @param tidyCensusAPIKey String for tidy_census::get_acs() api key
#' @param hexSize number specifying hex size (smaller value creates more smaller sized hexes but takes longer to load). Default = 0.01
#'
#' @return returns an sf data frame with 4 columns (hex_id, routes, uniqueRouteCount, geometry), where routes contains a character vector of routes in the hex and uniqueRouteCount counts the number of routes in each hex
#' @export
#' 
uniqueRoutesInHexTessalation <- function(gtfsObj, tidyCensusAPIKey = NULL, hexSize = 0.01) {
  
  # Define piping function
  `%>%` <- magrittr::`%>%`
  
  simpleRoutesAtStops <- gtfsFunctions::simpleRoutesAtStops(gtfsObj)%>%
                              sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
  
  # Create Rectangular Polygon Containing All Points For Hex Filling
  boundingBoxCoords <- rbind(c(min(gtfsObj$stops$stop_lon), min(gtfsObj$stops$stop_lat)),
                             c(min(gtfsObj$stops$stop_lon), max(gtfsObj$stops$stop_lat)),
                             c(max(gtfsObj$stops$stop_lon), max(gtfsObj$stops$stop_lat)),
                             c(max(gtfsObj$stops$stop_lon), min(gtfsObj$stops$stop_lat)),
                             c(min(gtfsObj$stops$stop_lon), min(gtfsObj$stops$stop_lat)))
  
  gtfsBBoxPolygon <-sf::st_polygon(list(boundingBoxCoords))%>%sf::st_sfc(crs = 4326)
  
  
  # Get Hex Tessalation
  size <- hexSize
  
  hex_points <- sp::spsample(sf::as_Spatial(gtfsBBoxPolygon), type = "hexagonal", cellsize = size)
  hex_grid <- sf::st_as_sf(sp::HexPoints2SpatialPolygons(hex_points, dx = size))
  hex_grid$hex_id <- seq(1:nrow(hex_grid))
  
  
  # Get Routes at Stops
  routesAtStops <- gtfsFunctions::namedList_RoutesAtStops(gtfsObj)
  
  # Get List of Routes at Stop in Better Format
  simpleRoutesAtStops$RouteList <- routesAtStops[simpleRoutesAtStops$stop_id]
  
  
  # Transit Count with HEx Grid
  simpleRoutesAtStops$HexId <- sp::over(sf::as_Spatial(simpleRoutesAtStops), sf::as_Spatial(hex_grid[,"hex_id"]))
  listOfHexIdRoutes <- split(simpleRoutesAtStops$RouteList, simpleRoutesAtStops$HexId)
  HEX_Ids <- names(listOfHexIdRoutes)
  hex2Routes <- data.frame(HEX_Ids)
  
  # Get Routes In Every Hex
  hex2Routes$routes <- sapply(HEX_Ids, function(x) unique(as.character(unlist(listOfHexIdRoutes[x]))))
  hex2Routes$uniqueRouteCount <- sapply(hex2Routes$routes, function(x) length(unique(x)))
  
  spatial_hex2Routes <- merge(hex_grid, hex2Routes, by.x = "hex_id", by.y = "HEX_Ids")
  
  return(spatial_hex2Routes)
}


