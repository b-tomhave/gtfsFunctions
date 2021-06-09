#' Calculate frequencies for gtfs frequencies.txt (frequency in seconds for each trip_id-service_id pair). Can take up to 20 seconds or so
#'
#' @param gtfs object from tidytransit read_gtfs()
#'
#' @return data.table conforming to gtfs frequencies.txt format with times as ITime (integer seconds)
#' @import data.table
#' @export
#' 
calculateFrequenciesByTripAndService <- function(gtfs) {
  departure_time = trip_id = stop_sequence = headway_secs = headway_prev = end_time = start_time = end_time = exact_times = NULL # due to NSE notes in R CMD check
  #`.` = function(...) NULL
  
  ## Load scheduled times
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  routes <- gtfs$routes
  
  # Convert to datatable
  setDT(trips)
  setDT(stop_times)
  
  # Join trips and stop times
  sch <- trips[stop_times, on = 'trip_id']
  
  ## Calculate headways by route and direction
  setkeyv(sch, c('service_id', 'departure_time'))
  
  # Make Sure departure time is in ITime transit format from which it can be converted to seconds
  sch[, departure_time := as.TransitTime(departure_time)]
  ## create frequencies.txt table where freq calculated per service_id, route_id and direction_id
  freqs <- sch[stop_sequence == 1, .(trip_id,
                                     start_time = departure_time,
                                     end_time = shift(departure_time, 1, type = 'lead'),
                                     secs = departure_time,
                                     headway_secs = c(diff(unclass(departure_time)), NA),
                                     headway_prev = c(NA, diff(unclass(departure_time))), exact_times = 0L
  ),
  keyby = c('service_id', 'route_id', 'direction_id')]
  
  # fix last trip of each service/route/direction
  freqs[is.na(headway_secs), headway_secs := headway_prev]
  # fix for only trip of service/route/direction
  freqs[is.na(headway_secs), headway_secs := 3600]
  freqs[is.na(end_time), end_time := start_time + headway_secs - 1L]
  freqs[, headway_prev := NULL]
  # If service starts before midnight and ends after (headway_secs is negative. Adjust this by adding 24hrs to the end time)
  freqs[, headway_secs := ifelse(headway_secs < 0, 
                                 headway_secs+ as.TransitTime("24:00:00"),
                                 headway_secs)]
  
  freqs[, .(trip_id, service_id, direction_id, start_time = start_time, end_time = end_time, headway_secs, exact_times)]
}


#' Calculate frequencies per route and time of day from tidytransit gtfs object
#'
#' @param gtfs object from tidytransit read_gtfs()
#'
#' @return data.table containing route, time period, and average headway
#' @import data.table
#' @export
#' 
calculateFrequenciesByRoute <- function(gtfs) {
  tripHeadways <- gtfsFunctions::calculateFrequenciesByTripAndService(gtfs)
  tripHeadways$headway_mins <- tripHeadways$headway_secs/60
  
  # Join trips.txt to this to get route_ids (only use route_ids to limit size)
  tripHeadways <- tripHeadways[dplyr::select(gtfs$trips, route_id, trip_id) , on = "trip_id"] # Join with route_ids
  
  # Reference table of seconds after midnight time od day (TOD) table for reference
  TOD <- data.table::data.table(BeginTime = c(0,14400,21600,32400,54000,66600,75600,86400),
                               EndTime = c(14400,21600,32400,54000,66600,75600,86400,100800),
                               Period = factor(c("Owl","Early","AM Peak","Midday",
                                                 "PM Peak","Evening", "Night","Owl"),
                                               levels = c("Early","AM Peak","Midday", "PM Peak",
                                                          "Evening", "Night", "Owl"),
                                               ordered = T))
  # Join time of day categorization to table
  tripHeadways <- tripHeadways[TOD, period := Period, on = .(start_time > BeginTime, start_time<= EndTime)]
  
  # Calculate average headway for route-direciton-time of day group. Round down to nearest minute
  tripHeadways <- tripHeadways[, .(avgHeadway_Mins = floor(mean(headway_mins))), by = c("route_id", "period", "direction_id")]

  #tripHeadways <- tripHeadways[, route_id := gtools::mixedsort(route_id)] # Isn't working right now
  
  dplyr::select(tripHeadways, route_id, direction_id, period, avgHeadway_Mins)%>%dplyr::distinct()
  
}