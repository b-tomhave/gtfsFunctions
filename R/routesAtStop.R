#' Function To Load GTFS Stops With Necessary Info
#'
#' @param gtfs gtfs object from tidytransit read_gtfs()
#'
#' @return data.table with one stop record for each route at a stop
#' @export
#' 
routeIDAtStops <- function(gtfs) { 
  data.table::setDT(gtfs$trips) # Convert to data.table
  data.table::setDT(gtfs$stop_times) # Convert to data.table
  
  #Load All Trips 
  shape_key <- gtfs$trips[, list(trip_id,route_id,direction_id,service_id)]%>%unique()
  data.table::setkey(shape_key, trip_id) # Set key for joining
  
  #Load Stop_Times df for stop sequence and arrival time at stops  (Takes a few seconds)
  stopTimes <- gtfs$stop_times[, list(trip_id,stop_id,stop_sequence)] %>% unique()
  setkey(stopTimes, trip_id, stop_id)
  
  # Inner join shape and stop times on trip_id
  stopTimes <- stopTimes[shape_key, on = 'trip_id']
  
  
  #Load Stops df for stop coordinates and name
  data.table::setDT(gtfs$stops) # Convert to data.table
  stopsDf<-gtfs$stops[, list(stop_id, stop_name, stop_lat, stop_lon)] %>% unique()
  data.table::setkey(stopsDf, stop_id)
  
  # Inner join stopsDf and stop times on stop_id
  stopsDf <- stopsDf[stopTimes, on = 'stop_id']
  
  stopsDf[, list(stop_id, route_id,  
                 stop_name, stop_lat, stop_lon)]%>%unique()
}

#' Function To Load GTFS Stops With Necessary Info (including direction_id and stop_sequence)
#'
#' @param gtfs gtfs object from tidytransit read_gtfs()
#'
#' @return data.table with one stop record for each route at a stop
#' @export
#' 
routeIDAtStopsWithDirAndSeq <- function(gtfs) { 
  # Define piping function
  `%>%` <- magrittr::`%>%`
  
  data.table::setDT(gtfs$trips) # Convert to data.table
  data.table::setDT(gtfs$stop_times) # Convert to data.table
  
  #Load All Trips 
  shape_key <- gtfs$trips[, list(trip_id, route_id, direction_id, service_id)]%>%unique()
  data.table::setkey(shape_key, trip_id) # Set key for joining
  
  #Load Stop_Times df for stop sequence and arrival time at stops  (Takes a few seconds)
  stopTimes <- gtfs$stop_times[, list(trip_id, stop_id, stop_sequence)] %>% unique()
  data.table::setkey(stopTimes, trip_id, stop_id)
  
  # Inner join shape and stop times on trip_id
  stopTimes <- stopTimes[shape_key, on = 'trip_id']
  
  
  #Load Stops df for stop coordinates and name
  data.table::setDT(gtfs$stops) # Convert to data.table
  stopsDf<-gtfs$stops[, list(stop_id, stop_name, stop_lat, stop_lon)] %>% unique()
  data.table::setkey(stopsDf, stop_id)
  
  # Inner join stopsDf and stop times on stop_id
  stopsDf <- stopsDf[stopTimes, on = 'stop_id']
  
  
  stopsDf[, list(stop_id, route_id,  
                 stop_name, stop_lat, stop_lon,
                 stop_sequence, direction_id)]%>%unique()
}


#' Function to take string of form '2,4,5,21,56' and convert to a list of form ('2','4','21','56')
#'
#' @param row Leave empty and use as function in lapply
#'
#' @return column of lists
#' @export
#' 
commaCharacter2List <- function(row) {
  return (as.list(el(strsplit(row, ","))))
}



#' Faster function to simplify gtfsFunctions::routeIDAtStops() to include only one record per stop with a column containing a list of all routes at stop.
#'
#' @param gtfsObject stops table from gtfsFunctions::routeIDAtStops() or stops.txt with one row per route-stop combo. MUST have "route_id" column
#'
#' @return Names list of all routes at stop where stop is name and routes are values 
#' @export
#' 
namedList_RoutesAtStops <- function(gtfsObject) { 
  stopTable_Long <- gtfsFunctions::routeIDAtStops(gtfsObject)
  return(split(stopTable_Long$route_id,stopTable_Long$stop_id))
}
  
  
#' Faster function to simplify gtfsFunctions::routeIDAtStops() to include only one record per stop with a column containing a list of all routes at stop.
#'
#' @param gtfsObject stops table from gtfsFunctions::routeIDAtStops() or stops.txt with one row per route-stop combo. MUST have "route_id" column
#'
#' @return Names list of all routes at stop where route_id is name and stop_ids are values 
#' @export
#' 
namedList_StopsOnRoutes <- function(gtfsObject) { 
  stopTable_Long <- gtfsFunctions::routeIDAtStops(gtfsObject)
  return(split(stopTable_Long$stop_id,stopTable_Long$route_id))
}

#' Function To Simplify gtfsFunctions::routeIDAtStops() to include only one record per stop with a column containing a list of all routes at stop.
#'
#' @param gtfsObject stops table from gtfsFunctions::routeIDAtStops() or stops.txt with one row per route-stop combo. MUST have "route_id" column
#'
#' @return data.table with one record per stop and new field 'routesAtStop' that is a list of all routes at stop
#' @export
#' 
simpleRoutesAtStops <- function(gtfsObject) { 
  stopTable_Long <- gtfsFunctions::routeIDAtStops(gtfsObject)
  routeId2FormattedName <- gtfsFunctions::routeId2routeShortName(gtfsObject$routes)
  simplifiedTable <- unique(stopTable_Long)[, routesAtStop := paste0(unique(as.character(routeId2FormattedName[route_id])),
                                                                     collapse = ','),
                                            by = stop_id]
  
  simplifiedTable <- subset(simplifiedTable, select = -c(route_id))%>%dplyr::distinct()
  
  simplifiedTable$routesAtStop <- sapply(strsplit(simplifiedTable$routesAtStop,','), function(x) toString(gtools::mixedsort(x)))
  
  # Reformat routesAtStop column to be a list
  #simplifiedTable$routesAtStop <- lapply(simplifiedTable$routesAtStop, commaCharacter2List)
  return(as.data.table(simplifiedTable))
}

#' Function to create named list for lookup that converts route_id name into formattedRouteShortName as the value
#' 
#' The new field formattedRouteName is the route_short_name (or route_id if no short_name). Useful for working with data where route_id doesn't directly correspond to same number that is public-facing
#'
#' @param gtfsRoutesDataTable gtfs routes.txt file in data.table format
#'
#' @return namedList
#' @export
#' 
routeId2routeShortName <- function(gtfsRoutesDataTable){
  # Create Formated Route Column (route_short_name unless empty in which case use route_id)
  namedList <- as.list(with(gtfsRoutesDataTable, ifelse(route_short_name == "",
                                                        as.character(route_id),
                                                        as.character(route_short_name))))
  names(namedList) <- as.vector(gtfsRoutesDataTable$route_id)
  return(namedList)
}

