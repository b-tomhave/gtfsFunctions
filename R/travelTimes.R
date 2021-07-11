#' Generate travel time summary between two stop_ids on a given route for a time range (default is 5AM-11:30PM)
#'
#' @param gtfs object from gtfsFunctions::formatGTFSObject()
#' @param beginStopID stop_id string to begin travel time calculations from
#' @param endStopID stop_id string to end travel time calculations at
#' @param routeID route_id string to calculate travel times for between stop_ids
#' @param startHHMMSSRange beginning HHMMSS string of timeframe to get travel times in (i.e. "15:30:00"). Default is 5AM
#' @param endHHMMSSRange end HHMMSS string of timeframe to get travel times in (i.e. "18:30:00"). Default is 11:30PM
#'
#' @return summary of travel times in specified time range
#' @export
#' 
generateTravelTimeSummary <- function(gtfs, beginStopID, endStopID,
                                      routeID, startHHMMSSRange = "00:05:00", endHHMMSSRange ="23:30:00") {
 
  # Define piping function
  `%>%` <- magrittr::`%>%`
  
  ## Load relevant gtfs data
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  
  # Convert to datatable
  data.table::setDT(trips)
  data.table::setDT(stop_times)
  
  # Join trips and stop times and only keep relevant stops
  sch <- trips[stop_times, on = 'trip_id'] %>% dplyr::filter(as.character(stop_id) %in% c(as.character(beginStopID), as.character(endStopID)) & as.character(route_id) == as.character(routeID))
  
  # Convert time format to integer seconds
  sch$arrival_time <- gtfsFunctions::as.TransitTime(sch$arrival_time)
  sch$departure_time <- gtfsFunctions::as.TransitTime(sch$departure_time)
  
  # Order by arrival_time from morning to evening
  sch <- data.table::setorder(sch, arrival_time)
  
  
  # Get unique trips in time frame 
  sch <- sch %>% dplyr::filter(arrival_time >= as.integer(gtfsFunctions::as.TransitTime(as.character(startHHMMSSRange))) & arrival_time <= as.integer(gtfsFunctions::as.TransitTime(as.character(endHHMMSSRange))))
  tripList <- unique(sch$trip_id)
  
  # Calculate travel times only if, for the given trip, both selected stops are served
  travelTimeList <- c()
  for (trip in tripList){
    singleTripData <- sch%>% dplyr::filter(trip_id == trip)
    if (nrow(singleTripData) > 1){
      # Check to make sure order of stops is correct
      if (as.numeric(singleTripData[ as.character(stop_id) == as.character(beginStopID), ]$stop_sequence) <= singleTripData[ as.character(stop_id) == as.character(endStopID), ]$stop_sequence){
        tt <- round(as.integer(singleTripData[-1,arrival_time] - singleTripData[-2,arrival_time])/60)
        travelTimeList <- c(travelTimeList, tt)
      }else{
        # Custom error if if statement is false
        stop('Error: Stop sequence of endStopID is greater than beginStopID. Switch their order.')
      }
    }
  }

  # Get summary of travel times
  if (length(travelTimeList) > 0){
    summary(travelTimeList)
  }else{
    #Custom error statement
    stop('Error: Data does not exist for the selected combination of stop ids time of day and route. Please change inputs.')
  }
}


#' Calculate travel time median between two stop_ids on a given route for a time range (default is 5AM-11:30PM)
#'
#' @param gtfs object from gtfsFunctions::formatGTFSObject()
#' @param beginStopID stop_id string to begin travel time calculations from
#' @param endStopID stop_id string to end travel time calculations at
#' @param routeID route_id string to calculate travel times for between stop_ids
#' @param startHHMMSSRange beginning HHMMSS string of timeframe to get travel times in (i.e. "15:30:00")
#' @param endHHMMSSRange end HHMMSS string of timeframe to get travel times in (i.e. "18:30:00")
#'
#' @return summary of travel times in specified time range
#' @export
#' 
calculateMedianTravelTime <- function(gtfs,beginStopID, endStopID,
                                      routeID, startHHMMSSRange = "00:05:00", endHHMMSSRange ="23:30:00") {
  
  as.numeric(gtfsFunctions::generateTravelTimeSummary(gtfs,beginStopID, endStopID,
                            routeID, startHHMMSSRange, endHHMMSSRange)['Median'])
  
  
}



