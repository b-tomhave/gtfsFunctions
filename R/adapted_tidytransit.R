#' Calculate frequencies per stop from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param startHHMMSS (optional) a string HH:MM:SS (default 06:00:00 (6 am))
#' @param endHHMMSS (optional) a string HH:MM:SS (default 22:00:00 (10 pm))
#' @param service_ids (optional) a string from the calendar dataframe identifying a particular service schedule.
#' @param by_route (optional) TRUE/FALSE indicating if segmenting frequencies by route. (defaul TRUE)
#'
#' @return tbl_df containing headways per stop
#' @import data.table
#' @export
#' 
get_stop_frequency <- function(gtfs_obj,
                               startHHMMSS = "06:00:00",
                               endHHMMSS = "22:00:00",
                               service_ids = c(),
                               by_route = T) {
  
  `:=` <- data.table::`:=`
  
  #Initialize number of departures and direction_id placeholder as null
  n_deps <- direction_id <- NULL
  
  # Convert data.frame to data table
  data.table::setDT(gtfs_obj$stop_times)
  data.table::setDT(gtfs_obj$trips)


  # If no specified service_ids then create frequencies for all service ids
  if(is.null(service_ids)) {
    # Get number of departures per trip
    dep_per_trip <- gtfs_obj$stop_times[, .(n_deps = data.table::uniqueN(trip_id)), by =.(trip_id)]
    
    # Get number of departures per service_id
    dep_per_service_id <- gtfs_obj$trips[dep_per_trip, on = 'trip_id'][, .(n_deps = sum(n_deps)), by = "service_id"]
    data.table::setorder(dep_per_service_id, -n_deps) # Sort desc by n_deps
    
    
    # Select service_id with greatest number of departures
    service_ids <- dep_per_service_id$service_id[1]
  }

  # Get trips on selected service_ids
  trips <- gtfs_obj$trips[service_id %in% service_ids]

  # Get stop times on selected service_id trips 
  stop_times <- gtfs_obj$stop_times[trip_id %in% trips$trip_id]
  
  # Set stop_times time fields in transit time (integer sec past midnight)
  stop_times$arrival_time <- lubridate::hms(stop_times$arrival_time)
  stop_times$departure_time <- lubridate::hms(stop_times$departure_time)

  # Further filter Stop times to only be those in time period
  stop_times <- stop_times[departure_time >= as.numeric(lubridate::hms(startHHMMSS)) & arrival_time <= as.numeric(lubridate::hms(endHHMMSS))]
  stop_times <- unique(stop_times[trips[, c("trip_id", "route_id", "direction_id", "service_id")], on = .(trip_id)])
  
  # find number of departure per stop_id, route_id, service_id
  # If aggregated by route_id
  if(by_route == T) {
    freq <- stop_times[, .(n_departures = .N),  by = .(stop_id, route_id, direction_id, service_id)]
  } else {
    freq <- stop_times[, .(n_departures = .N),  by = .(stop_id, service_id)]
  }
  #calculate average headway
  durationMinutes <- (as.numeric(gtfsFunctions::as.TransitTime(endHHMMSS))-as.numeric(gtfsFunctions::as.TransitTime(startHHMMSS)))/60
  freq$headwayMinutes <- round(durationMinutes / freq$n_departures)
  
  freq
}



#' Calculate frequencies per route from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param startHHMMSS (optional) a string HH:MM:SS (default 06:00:00 (6 am))
#' @param endHHMMSS (optional) a string HH:MM:SS (default 22:00:00 (10 pm))
#' @param service_ids (optional) a string from the calendar dataframe identifying a particular service schedule.
#' @param by_route (optional) TRUE/FALSE indicating if segmenting frequencies by route. (defaul TRUE)
#'
#' @return tbl_df headways per route
#' @import data.table
#' @export
#' 
get_route_frequency <- function(gtfs_obj,
                                startHHMMSS = "06:00:00",
                                endHHMMSS = "22:00:00",
                                service_ids = c(),
                                by_route = T) {

  # Get Frequency of all routes at all stops they servce
  stops_frequency <- get_stop_frequency(gtfs_obj, startHHMMSS, 
                                        endHHMMSS, service_ids, by_route)  
  
  # For each route take average of frequencies at all stops on that route
  if (dim(stops_frequency)[[1]]!=0) {
    routes_frequency <- stops_frequency %>%
      dplyr::group_by(route_id, direction_id) %>%
      dplyr::summarise(total_departures = sum(n_departures),
                       median_headways = 
                         as.integer(round(median(headwayMinutes),0)),
                       mean_headways = 
                         as.integer(round(mean(headwayMinutes),0)),
                       st_dev_headways = 
                         round(sd(headwayMinutes),2),
                       stop_count = dplyr::n())
  } else {
    warning("Failed to calculate frequency, try passing a service_id from calendar_df.")
  }
  return(routes_frequency)
}

# gtfsPath_Jul <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_June21.zip"
# gtfsPath_Jul2 <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\ETS_Edmonton_July21.zip"
# # 
# gtfs4 <- formatGTFSObject(gtfsPath_Jul2)
# aweseom <- tidytransit::read_gtfs(gtfsPath_Jul2)
# test <- gtfsFunctions::routeIDAtStops(gtfs3)
# 
# ptm <- proc.time()
# # 4 Seconds
# route_frequency <-  gtfsFunctions::get_route_frequency(gtfs3,
#                                         startHHMMSS = "09:00:00",
#                                         endHHMMSS = "12:00:00",
#                                         service_ids = NULL,
#                                         by_route = T)
# proc.time() - ptm
# route_frequency <- route_frequency[gtools::mixedorder(route_frequency$route_id),]
