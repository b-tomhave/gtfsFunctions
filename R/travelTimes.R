#' Generate travel time summary between two stop_ids on a given route
#'
#' @param gtfs object from gtfsFunctions::formatGTFSObject()
#' @param beginStopID stop_id string to begin travel time calculations from
#' @param endStopID stop_id string to end travel time calculations at
#' @param routeID route_id string to calculate travel times for between stop_ids
#' @param startHHMMSSRange beginning HHMMSS string of timeframe to get travel times in (i.e. "15:30:00")
#' @param endHHMMSSRange end HHMMSS string of timeframe to get travel times in (i.e. "18:30:00")
#'
#' @return summary of travel times in specified time range
#' @import data.table
#' @import dplyr
#' @export
#' 
generateTravelTimeSummary <- function(gtfs,beginStopID, endStopID,
                                      routeID, startHHMMSSRange, endHHMMSSRange) {
 
  ## Load relevant gtfs data
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  
  # Convert to datatable
  setDT(trips)
  setDT(stop_times)
  
  # Join trips and stop times and only keep relevant stops
  sch <- trips[stop_times, on = 'trip_id']%>%filter(as.character(stop_id) %in% c(beginStopID, endStopID) & as.character(route_id) == as.character(routeID))
  
  # Convert time format to integer seconds
  sch$arrival_time <- gtfsFunctions::as.TransitTime(sch$arrival_time)
  sch$departure_time <- gtfsFunctions::as.TransitTime(sch$departure_time)
  
  # Order by arrival_time from morning to evening
  sch <- setorder(sch, arrival_time)
  
  
  # Get unique trips in time frame 
  sch <- sch%>%filter(arrival_time >= as.integer(gtfsFunctions::as.TransitTime(as.character(startHHMMSSRange))) & arrival_time <= as.integer(gtfsFunctions::as.TransitTime(as.character(endHHMMSSRange))))
  tripList <- unique(sch$trip_id)
  
  # Calculate travel times only if, for the given trip, both selected stops are served
  travelTimeList <- c()
  for (trip in tripList){
    test <- sch%>%filter(trip_id == trip)
    if (nrow(test) > 1){
      tt <- round(as.integer(test[-1,arrival_time] - test[-2,arrival_time])/60)
      travelTimeList <- c(travelTimeList, tt)
    }
  }
  # # Get avg travel time
  # print(mean(travelTimeList))
  # 
  # Get summary of travel times
  summary(travelTimeList)
  
  
 
}



#' Calculate travel time median between two stop_ids on a given route
#'
#' @param gtfs object from gtfsFunctions::formatGTFSObject()
#' @param beginStopID stop_id string to begin travel time calculations from
#' @param endStopID stop_id string to end travel time calculations at
#' @param routeID route_id string to calculate travel times for between stop_ids
#' @param startHHMMSSRange beginning HHMMSS string of timeframe to get travel times in (i.e. "15:30:00")
#' @param endHHMMSSRange end HHMMSS string of timeframe to get travel times in (i.e. "18:30:00")
#'
#' @return summary of travel times in specified time range
#' @import data.table
#' @import dplyr
#' @export
#' 
calculateMedianTravelTime <- function(gtfs,beginStopID, endStopID,
                                      routeID, startHHMMSSRange, endHHMMSSRange) {
  
  as.numeric(gtfsFunctions::generateTravelTimeSummary(gtfs,beginStopID, endStopID,
                            routeID, startHHMMSSRange, endHHMMSSRange)['Median'])
  
  
}



