#' Convert GTFS Route, Trip, and Shape File to SF Linestring Object
#'
#' This function loads and joins gtfs route.txt, trip.txt, and shape.txt files
#' identifies route colors (if present) and returns an sf object
#' of linestring features for each shape. Each output record
#' contains both a shape_id and route_id for easy filtering.
#'
#' @param routesDf_GTFS routes.txt gtfs data.table file
#' @param tripsDf_GTFS trips.txt gtfs data.table file
#' @param shapesDf_GTFS shapes.txt gtfs data.table file
#' @return A sf object of route geometry linestrings
#' @importFrom magrittr %>%
#' @import data.table
#' @export

gtfs2RouteLines <- function(routesDf_GTFS, tripsDf_GTFS, shapesDf_GTFS){
  # Make sure data.table knows we know we're using it (solution to bug)
  .datatable.aware = TRUE
  
  # Convert data to data.table if not already in that format
  if (data.table::is.data.table(routesDf_GTFS) == F){
    data.table::setDT(routesDf_GTFS)}
  if (data.table::is.data.table(tripsDf_GTFS) == F){
    data.table::setDT(tripsDf_GTFS)}
  if (data.table::is.data.table(shapesDf_GTFS) == F){
    data.table::setDT(shapesDf_GTFS)}
  
  # Get original shapes as sf from tidytransit package
  shapeSpatial <- tidytransit::shapes_as_sf(shapesDf_GTFS)
  
  # Load Route and Shape ID From Trips Datafame
  shape_key <- unique(tripsDf_GTFS[, .(route_id, shape_id)])
  data.table::setkey(shape_key,route_id) # Set Key For Joining To Occur On "route_id"

  # Create or load basic route_color field
  if ( !is.null(routesDf_GTFS$route_color) ) { # extract if they exist
    shape_key <- shape_key[routesDf_GTFS[, .(route_color, route_id)], on = 'route_id']
  }else { # planB: build a pal from my pallette 'd3'
    shape_key <- shape_key[,route_color := rep("",
                                 length.out = nrow(shape_key))]}
  
  
  # Modify route colors
  # For each route group check if color exists. If not create random hex color. If one exists but doesn't have a pound sign add it, otherwise keep color
  shape_key <- shape_key[, route_color:= ifelse(route_color == "", 
                                                 sample(RColorBrewer::brewer.pal(8, "Dark2"),1),
                                                 ifelse(substring(route_color, 1, 1) == "#",
                                                        sample(RColorBrewer::brewer.pal(8, "Dark2"),1),
                                                        paste0("#", route_color))),
                         by = route_id]

  shapeSpatial<- shapeSpatial%>%dplyr::inner_join(shape_key)
  return (shapeSpatial)
}
