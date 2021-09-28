#' Calculate frequencies per stop from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param by_TOD (optional) TRUE/FALSE indicating if segmenting frequencies by time of day periods or overall. If by_TOD == T don't provide start and end HHMMSS
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
                               by_TOD = T,
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
  stop_times <- na.omit(gtfs_obj$stop_times[trip_id %in% trips$trip_id])

  # Change output based on if using TOD or custom time range
  if (by_TOD == T){
    # Get TOD Associated with stop arrival and join to stop times
    stop_times <- stop_times[gtfsFunctions::todTable(), `:=`(period = Period, periodMinutes = periodMinutes), on = .(arrival_time > BeginTime, departure_time <= EndTime)]
    
  }else{
    # Get Stop Times in Custom Time Range
    stop_times <- stop_times[departure_time >= as.numeric(lubridate::hms(startHHMMSS)) & arrival_time <= as.numeric(lubridate::hms(endHHMMSS))] 
    
    # Create periodMinutes Field indicating the duration of custom period
    stop_times <- stop_times[, periodMinutes := (as.numeric(lubridate::hms(endHHMMSS)) - as.numeric(lubridate::hms(startHHMMSS)))/60]
    stop_times <- stop_times[, period := "custom"]

  }

  # Further filter Stop times to only be those in time period
  # Remove NAs that popup on join
  stop_times <- na.omit(unique(stop_times[trips[, c("trip_id", "route_id", "direction_id", "service_id")], on = .(trip_id)]))

  # find number of departure per stop_id, route_id, service_id
  # If aggregated by route_id
  if(by_route == T) {
    freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes),  by = .(stop_id, route_id, direction_id, service_id, period)]
  } else {
    freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes),  by = .(stop_id, service_id, period)]
  }
  
  nrow(freq)
  #calculate average headway
  #durationMinutes <- (as.numeric(gtfsFunctions::as.TransitTime(endHHMMSS))-as.numeric(gtfsFunctions::as.TransitTime(startHHMMSS)))/60
  freq$headwayMinutes <- round(freq$periodMinutes / freq$n_departures)
  
  freq %>% distinct()
}



#' Calculate frequencies per route from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param by_TOD (optional) TRUE/FALSE indicating if segmenting frequencies by time of day periods or overall. If by_TOD == T don't provide start and end HHMMSS
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
                                by_TOD = T,
                                startHHMMSS = "06:00:00",
                                endHHMMSS = "22:00:00",
                                service_ids = c(),
                                by_route = T) {

  # Get Frequency of all routes at all stops they servce
  stops_frequency <- get_stop_frequency(gtfs_obj, by_TOD, startHHMMSS, 
                                        endHHMMSS, service_ids, by_route)  
  
  # For each route take average of frequencies at all stops on that route
  if (dim(stops_frequency)[[1]]!=0) {
    # If grouping by time of day then make sure to incude as a variable
    if (by_TOD == T){
      routes_frequency <- stops_frequency %>%
        dplyr::group_by(route_id, direction_id, period) %>%
        dplyr::summarise(total_departures = sum(n_departures),
                         median_headways = 
                           as.integer(round(median(headwayMinutes),0)),
                         mean_headways = 
                           as.integer(round(mean(headwayMinutes),0)),
                         st_dev_headways = 
                           round(sd(headwayMinutes),2),
                         stop_count = dplyr::n())
    }else{
      routes_frequency <- stops_frequency %>%
        distinct() %>%
        dplyr::group_by(route_id, direction_id) %>%
        dplyr::summarise(total_departures = sum(n_departures),
                         median_headways = 
                           as.integer(round(median(headwayMinutes),0)),
                         mean_headways = 
                           as.integer(round(mean(headwayMinutes),0)),
                         st_dev_headways = 
                           round(sd(headwayMinutes),2),
                         stop_count = dplyr::n())
    }
  } else {
    warning("Failed to calculate frequency, try passing a service_id from calendar_df.")
  }
  return(routes_frequency)
}

# gtfsPath_Jul <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_June21.zip"
# gtfsPath_Jul2 <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\ETS_Edmonton_July21.zip"
# #
# gtfs_obj <- gtfsFunctions::formatGTFSObject(gtfsPath_Jul)
# # aweseom <- tidytransit::read_gtfs(gtfsPath_Jul2)
# # test <- gtfsFunctions::routeIDAtStops(gtfs3)
# # 
# library(dplyr)
# ptm <- proc.time()
# # 1 Second
# 
# stops <- get_stop_frequency(gtfs_obj,
#                                                   by_TOD = F,
#                                                   service_ids = NULL,
#                                                   by_route = T) %>%na.omit()
# 
# ptm <- proc.time()
# route_frequency2 <-  get_route_frequency(gtfs_obj,
#                                           by_TOD = T,
#                                           # startHHMMSS = "13:00:00",
#                                           # endHHMMSS = "13:30:00",
#                                           service_ids = NULL,
#                                           by_route = T)
# proc.time() - ptm
# route_frequency <- route_frequency[gtools::mixedorder(route_frequency$route_id),]
