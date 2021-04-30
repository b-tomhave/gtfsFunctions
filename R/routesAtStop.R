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