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
  
  if (data.table::is.data.table(routesDf_GTFS) == F) stop("routesDf_GTFS object must be of type 'data.table'")
  if (data.table::is.data.table(tripsDf_GTFS) == F) stop("tripsDf_GTFS object must be of type 'data.table'")
  if (data.table::is.data.table(shapesDf_GTFS) == F) stop("shapesDf_GTFS object must be of type 'data.table'")
  
  # Get original shapes as sf from tidytransit package
  shapeSpatial <- tidytransit::shapes_as_sf(shapesDf_GTFS)
  
  # Load Route and Shape ID From Trips Datafame
  shape_key <- unique(tripsDf_GTFS[, .(route_id, shape_id)])
  data.table::setkey(shape_key,route_id) # Set Key For Joining To Occur On "route_id"
  
  # Assign color to route
  if ( !is.null(routesDf_GTFS$route_color) ) { # extract if they exist
    shape_key[routesDf_GTFS[, .(route_color, route_id)], on = 'route_id']
  }else { # planB: build a pal from my pallette 'd3'
    shape_key[,route_color := rep(ggsci::pal_d3()(10),
                                 length.out = nrow(shape_key))]}
  
  #Load Route Name and ID From Routes Datafame
  # route_key <- routesDf_GTFS[, .(route_id, route_short_name)]
  # data.table::setkey(route_key,route_id) # Set Key For Joining To Occur On "route_id"
  # route_key <- route_key[shape_key] # Inner Join routes and shapes on route_id
  # 
  #shape_key$route_id_NoDash <- as.character(sapply(strsplit(as.character(shape_key$route_id),"-"),'[',1)) # Remove dash

  
  # ## build the sf object ----
  # # exctract lon/lat values as matrix to build linestrings for each "shape_id"
  # sfc <- shapesDf_GTFS %>% # long data frame
  #   split(.$shape_id) %>% # list of shorted data framee, one per shape
  #   purrr::map(~ dplyr::select(., shape_pt_lon, shape_pt_lat) %>% # order maters, lon-1st lat-2nd
  #                as.matrix() %>% # coherce for st_linestrings happiness
  #                sf::st_linestring()) %>%
  #   sf::st_sfc(crs = 4326) # bundle all shapes into a collection
  # 
  # 
  # # add collection on and convert to sf
  # outputLines <- unique(shapesDf_GTFS$shape_id) %>%
  #   sort() %>% # sort to match with names(sfc); split()'s factor-cohercion alpha sorts
  #   sf::st_sf(shape_id = ., geometry = sfc) %>%
  #   dplyr::inner_join(route_key)#%>%dplyr::distinct() distinct is too slow

  shapeSpatial<- shapeSpatial%>%dplyr::inner_join(shape_key)
  return (shapeSpatial)
}
