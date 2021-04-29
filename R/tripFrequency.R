#' Calculate frequencies for gtfs frequencies.txt (frequency in seconds for each trip_id). Can take up to 20 seconds or so
#'
#' @param gtfs object from tidytransit read_gtfs()
#'
#' @return data.table conforming to gtfs frequencies.txt format with times as ITime (integer seconds)
#' @import data.table
#' @export
#' 
calculateFrequenciesByTrip <- function(gtfs) {
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
  ## create frequencies.txt table
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
  
  freqs[, .(trip_id, start_time = start_time, end_time = end_time, headway_secs, exact_times)]
}