#' Function To Load GTFS Stops With Necessary Info
#'
#' @param gtfs gtfs object from tidytransit read_gtfs()
#'
#' @return data.table with one stop record for each route at a stop
#' @export
#' 
routesAtStops <- function(gtfs) { 
  setDT(gtfs$trips) # Convert to data.table
  setDT(gtfs$stop_times) # Convert to data.table
  
  #Load All Trips 
  shape_key <- gtfs$trips[, list(trip_id,route_id,direction_id,service_id)]%>%unique()
  setkey(shape_key, trip_id) # Set key for joining
  
  #Load Stop_Times df for stop sequence and arrival time at stops  (Takes a few seconds)
  stopTimes <- gtfs$stop_times[, list(trip_id,stop_id,stop_sequence)] %>% unique()
  setkey(stopTimes, trip_id, stop_id)
  
  # Inner join shape and stop times on trip_id
  stopTimes <- stopTimes[shape_key, on = 'trip_id']
  
  
  #Load Stops df for stop coordinates and name
  setDT(gtfs$stops) # Convert to data.table
  stopsDf<-gtfs$stops[, list(stop_id, stop_code, stop_name, stop_lat, stop_lon)] %>% unique()
  setkey(stopsDf, stop_id)
  
  # Inner join stopsDf and stop times on stop_id
  stopsDf <- stopsDf[stopTimes, on = 'stop_id']
  
  stopsDf[, list(stop_id, route_id, stop_code,  
                 stop_name, stop_lat, stop_lon)]%>%unique()
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


#' Function To Simplify gtfsFunctions::routesAtStops() to include only one record per stop with a column containing a list of all routes at stop.
#'
#' @param stopsTable stops table from gtfsFunctions::routesAtStops() or stops.txt with one row per route-stop combo. MUST have "route_id" column
#' @param route_ColumnName route_id column name character
#'
#' @return data.table with one record per stop and new field 'routesAtStop' that is a list of all routes at stop
#' @export
#' 
simplifyRoutesAtStops <- function(stopsTable) { 
  simplifiedTable <- unique(stopsTable)[, routesAtStop := paste0(route_id, collapse = ','), by = stop_id]
    # stopsTable%>%unique()%>% 
    #                     dplyr::group_by(stop_id) %>% 
    #                     #dplyr::arrange(gtools::mixedsortroute_id)%>%
    #                     dplyr::mutate(routesAtStop = paste0(route_id, collapse = ",")) #%>%#Create new field with string of all routes serving stop
    #                     #ungroup()
  
    simplifiedTable <- subset(simplifiedTable, select = -c(route_id))%>%dplyr::distinct()
  
  simplifiedTable$routesAtStop <- sapply(strsplit(simplifiedTable$routesAtStop,','), function(x) toString(gtools::mixedsort(x)))
  
  # Reformat routesAtStop column to be a list
  #simplifiedTable$routesAtStop <- lapply(simplifiedTable$routesAtStop, commaCharacter2List)
  return(as.data.table(simplifiedTable))
}

