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


# test <- gtfsFunctions::calculateFrequenciesByRoute(data)
# rt3 <- test[route_id == "3"]
# 
# 
# 
# library(gtfsFunctions) #devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE)
# library(data.table)
# library(dplyr)
# library(gtools)
# gtfsPath <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_Aug21_2021.zip"
# gtfsPath_Jul <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_June21.zip"
# data <-  gtfsio::import_gtfs(gtfsPath_Jul,
#                              files = c('agency', 'stops', 'routes', 'trips',
#                                        'stop_times', #'calendar', 'calendar_dates',
#                                        'shapes'))
# 
# library(tidytransit)
# library(sf)
# gtfs <- tidytransit::read_gtfs(gtfsPath_Jul)
# gtfs <- set_servicepattern(gtfs)
# shp1 <- shapes_as_sf(gtfs$shapes)
# shp1 <- st_transform(shp1, crs=4326)
# shp1$length <- st_length(shp1)
# shp2 <- shp1 %>% 
#   as.data.frame() %>% 
#   select(shape_id,length,-geometry) 
# 
# this <- gtfs$.
# gtfs <- set_servicepattern(gtfs)
# # service ids used
# n_services <-  length(unique(gtfs$trips$service_id)) # 70
# 
# # unique date patterns 
# n_servicepatterns <- length(unique(gtfs$.$servicepatterns$servicepattern_id)) # 7))
# 
# date_servicepattern_table <- gtfs$.$dates_servicepatterns %>% left_join(calendar, by = "date")
# 
# library(ggplot2)
# ggplot(date_servicepattern_table) + theme_bw() + 
#   geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) + 
#   scale_x_date(breaks = scales::date_breaks("1 month")) + theme(legend.position = "bottom")
# 
# 
# # Need to deal with hlidays
# suggest_servicepattern_name = function(dates, calendar) {
#   servicepattern_calendar = tibble(date = dates) %>% left_join(calendar, by = "date")
#   
#   # all normal dates without holidays
#   calendar_normal = servicepattern_calendar 
#   
#   # create a frequency table for all calendar dates without holidays
#   weekday_freq = sort(table(calendar_normal$weekday), decreasing = T)
#   n_weekdays = length(weekday_freq)
#   
#  
#   if(n_weekdays == 7) {
#     pattern_name = "Every day"
#   }
#   # Single day service
#   else if(n_weekdays == 1) {
#     wd = names(weekday_freq)[1]
#     # while paste0(weekday, "s") is easier, this solution can be used for other languages
#     pattern_name = c("Sunday"  = "Sundays", 
#                      "Monday"    = "Mondays", 
#                      "Tuesday"   = "Tuesdays", 
#                      "Wednesday" = "Wednesdays",
#                      "Thursday"  = "Thursdays",  
#                      "Friday"    = "Fridays",  
#                      "Saturday"  = "Saturdays")[wd]
#   } 
#   # Weekday Service
#   else if(n_weekdays == 5 && 
#           length(intersect(names(weekday_freq), 
#                            c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) == 5) {
#     pattern_name = "Weekdays"
#   }
#   # Weekend
#   else if(n_weekdays == 2 && 
#           length(intersect(names(weekday_freq), c("Saturday", "Sunday"))) == 2) {
#     pattern_name = "Weekends"
#   }
#   # Multiple weekdays that appear regularly
#   else if(n_weekdays >= 2 && (max(weekday_freq) - min(weekday_freq)) <= 1) {
#     wd = names(weekday_freq)
#     ordered_wd = wd[order(match(wd, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))]
#     pattern_name = paste(ordered_wd, collapse = ", ")
#   } 
#   # default
#   else {
#     pattern_name = paste(weekday_freq, names(weekday_freq), sep = "x ", collapse = ", ")
#   }
#   
#   
#   pattern_name <- paste0(pattern_name, " (", min(dates), " - ", max(dates), ")") 
#   
#   return(pattern_name)
# }
# 
# servicepattern_names = gtfs$.$dates_servicepatterns %>% 
#   group_by(servicepattern_id) %>% summarise(
#     servicepattern_name = suggest_servicepattern_name(date, calendar)
#   )
# 
# print(servicepattern_names)
# 
# gtfs$.$servicepatterns %>% 
#   filter(servicepattern_id == 's_7fcfe47') %>% 
#   pull(service_id)
# 
# # Plot Calendar for each service pattern
# dates = gtfs$.$dates_servicepatterns
# dates$wday <- lubridate::wday(dates$date, label = T, abbr = T, week_start = 7)
# dates$week_nr <- lubridate::week(dates$date)
# 
# dates <- dates %>% group_by(week_nr) %>% summarise(week_first_date = min(date)) %>% right_join(dates, by = "week_nr")
# 
# week_labels = dates %>% select(week_nr, week_first_date) %>% unique()
# 
# ggplot(dates) + theme_bw() +
#   geom_tile(aes(x = wday, y = week_nr), color = "#747474") +
#   scale_x_discrete(drop = F) +
#   scale_y_continuous(trans = "reverse", labels = week_labels$week_first_date, breaks = week_labels$week_nr) +
#   theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(x = NULL, y = "Date of Sundays") +
#   facet_wrap(~servicepattern_id, nrow = 1)
# 
# # Identifies if weekday or not
# calendar = tibble(date = unique(gtfs$.$dates_services$date)) %>% 
#   mutate(
#     weekday = (function(date) {
#       c("Sunday", "Monday", "Tuesday", 
#         "Wednesday", "Thursday", "Friday", 
#         "Saturday")[as.POSIXlt(date)$wday + 1]
#     })(date)
#   )
# 
# service_pattern_summary <- gtfs$trips %>%
#   left_join(gtfs$.$servicepatterns, by="service_id") %>% 
#   left_join(shp2, by="shape_id") %>%
#   left_join(gtfs$stop_times, by="trip_id") %>% 
#   group_by(servicepattern_id) %>% 
#   summarise(trips = n(), 
#             routes = n_distinct(route_id),
#             total_distance_per_day_km = sum(as.numeric(length), 
#                                             na.rm=TRUE)/1e3,
#             route_avg_distance_km = (sum(as.numeric(length),
#                                          na.rm=TRUE)/1e3)/(trips*routes),
#             stops=(n_distinct(stop_id)/2)) %>%
#   left_join(servicepattern_names, by = "servicepattern_id")
# 
# summary(gtfs)
# 
# 
# 
# 
# 
# # get the id of the first stop in the trip's stop sequence
# first_stop_id <- gtfs$stop_times %>% 
#   group_by(trip_id) %>% 
#   summarise(stop_id = stop_id[which.min(stop_sequence)])
# 
# 
# # join with the stops table to get the stop_name
# first_stop_names <- left_join(first_stop_id, gtfs$stops, by="stop_id")
# # rename the first stop_name as trip_origin
# trip_origins <- first_stop_names %>% select(trip_id, trip_origin = stop_name)
# 
# # join the trip origins back onto the trips
# gtfs$trips <- left_join(gtfs$trips, trip_origins, by = "trip_id")
# 
# 
# # get the last id of the trip's stop sequence
# trip_dest <- gtfs$stop_times %>% 
#   group_by(trip_id) %>% 
#   summarise(stop_id = stop_id[which.max(stop_sequence)]) %>% 
#   left_join(gtfs$stops, by="stop_id") %>% select(trip_id, trip_destination = stop_name)
# 
# # join the trip dests back onto the trips
# gtfs$trips <- this <- left_join(gtfs$trips, trip_dest, by = "trip_id")
# 
# # Trips with origin and destination stop
# gtfs$trips %>%
#   select(route_id, trip_origin) %>%
#   head()
# 
# stop_ids <- gtfs$stops %>% 
#   filter(stop_id == 56112) %>% 
#   select(stop_id)
# 
# departures <- stop_ids %>% 
#   inner_join(gtfs$stop_times %>% 
#                select(trip_id, arrival_time, 
#                       departure_time, stop_id), 
#              by = "stop_id")
# 
# departures <- departures %>% 
#   left_join(gtfs$trips %>% 
#               select(trip_id, route_id, 
#                      service_id, trip_headsign, 
#                      trip_origin), 
#             by = "trip_id") 
# 
# 
# departures <- departures %>% 
#   left_join(gtfs$routes %>% 
#               select(route_id, 
#                      route_short_name), 
#             by = "route_id")
# 
# # Get departure table
# dept56112 <- departures %>% 
#   select(arrival_time,
#          departure_time,
#          trip_headsign,trip_origin,
#          route_id) %>%
#   head() %>%
#   knitr::kable()
# 
# services_on_210623 <- gtfs$.$dates_services %>% 
#   filter(date == "2021-06-23") %>% select(service_id)
# 
# departures_56112_June23<- departures %>% 
#   inner_join(services_on_210623, by = "service_id")
# 
# departures_56112_June23 %>%
#   arrange(departure_time, stop_id, route_id) %>% 
#   select(arrival_time, departure_time, stop_id, route_id, trip_headsign) %>% 
#   #filter(departure_time >= hms::hms(hours = 7)) %>% 
#   #filter(departure_time < hms::hms(hours = 7, minutes = 10)) %>% 
#   knitr::kable()
# 
# 
# 
# get_route_frequency <- function(gtfs_obj,
#                                 start_hour=6,
#                                 end_hour=22,
#                                 service_ids = c(),
#                                 by_route = T) {
# 
#   stops_frequency <- get_stop_frequency(gtfs_obj, start_hour, 
#                                         end_hour, service_ids, by_route)  
#   
#   if (dim(stops_frequency)[[1]]!=0) {
#     routes_frequency <- stops_frequency %>%
#       dplyr::group_by(route_id) %>%
#       dplyr::summarise(total_departures = sum(n_departures),
#                        median_headways = 
#                          as.integer(round(median(mean_headway/60),0)),
#                        mean_headways = 
#                          as.integer(round(mean(mean_headway/60),0)),
#                        st_dev_headways = 
#                          round(sd(mean_headway/60),2),
#                        stop_count = dplyr::n())
#     
#     # routes_frequency <- routes_frequency[gtools::mixedorder(route_id),]
#   } else {
#     warning("Failed to calculate frequency, try passing a service_id from calendar_df.")
#   }
#   return(routes_frequency)
# }
# 
# 
# get_route_frequency
# 
# tsetTrips <- gtfs$trips %>%
#   filter(service_id %in% service_ids) %>%
#   group_by(service_id, route_id) %>%
#   summarise(count = n())
# 
# am_freq <- get_stop_frequency(gtfs, start_hour = 6, end_hour = 10, service_ids = service_ids, by_route = T)
# 
# # Data frame of route_type to name
# tidytransit::route_type_names
# summary(gtfs)
# a_line_stops <- am_freq %>% 
#   filter(route_id==921 & direction_id==0) %>%
#   left_join(gtfs$stops, by ="stop_id")
# 
# am_route_freq <- get_route_frequency(gtfs, service_ids = , start_hour = 6, end_hour = 10) 
# summary(a_line_stops$mean_headway)/60
# 
# spatial <- gtfs_as_sf(gtfs, skip_shapes = T, crs = NULL, quiet = TRUE)
# routes_sf <- get_route_geometry(spatial)
# plot(routes_sf, route_ids = 921)
# 
# 2+3
# plot(spatial$shapes)
# test <- tidytransit::get_stop_frequency(gtfs, start_hour = 6, end_hour = 22,
#                                 by_route = T)
# 
# 
# this <- test[test$route_id == "3", ]
# # Have option to select by direction/headsign
# 
# ## Load scheduled times
# trips <- data$trips
# stop_times <- data$stop_times
# routes <- data$routes
# 
# 
# # Convert to datatable
# setDT(trips)
# setDT(stop_times)
# 
# # Join trips and stop times
# sch <- trips[stop_times, on = 'trip_id']
# 
# 
# # Get Most Frequent Stop Per Route, Direction, Service, and Trip Headsign combo
# # This shoud find the frequency of the highest frequency part of the route
# # Function to calculate single mode value (i.e. most frequent stop)
# SingleMode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# sch[, stopIdMode := SingleMode(stop_id), by = .(route_id, direction_id, service_id, trip_headsign)]
# 
# # Subset By Stop Mode
# mostCommonStop_Sch <- sch[stop_id == stopIdMode]
# 
# # sch <- sch[, .SD[which.max(stop_id)], by= .(route_id, direction_id, service_id, trip_headsign)]
# aLine <- mostCommonStop_Sch[ route_id == "921"]%>%unique()
# 
# # Get Times in integer seconds past midnight
# mostCommonStop_Sch$arrival_time <- gtfsFunctions::as.TransitTime(mostCommonStop_Sch$arrival_time)
# mostCommonStop_Sch$departure_time <- gtfsFunctions::as.TransitTime(mostCommonStop_Sch$departure_time)
# 
# # Add Time of Day By Stop_time
# # Reference table of seconds after midnight time od day (TOD) table for reference
# TOD <- data.table::data.table(BeginTime = c(0,14400,21600,32400,54000,66600,75600,86400),
#                               EndTime = c(14400,21600,32400,54000,66600,75600,86400,100800),
#                               Period = factor(c("Owl","Early","AM Peak","Midday",
#                                                 "PM Peak","Evening", "Night","Owl"),
#                                               levels = c("Early","AM Peak","Midday", "PM Peak",
#                                                          "Evening", "Night", "Owl"),
#                                               ordered = T))
# 
# # Create Minutes Per TOD
# TOD[, periodMinutes := (EndTime - BeginTime)/60]
# 
# # Join time of day categorization to table
# mostCommonStop_Sch[TOD, `:=` (period = Period, periodMinutes = periodMinutes), on = .(arrival_time > BeginTime, departure_time<= EndTime)]
# 
# 
# # Get Unique Trip Counts Per TOD Period Per Service, Headsign, Direction, and Route
# mostCommonStop_Sch[, tripCount := uniqueN(trip_id), by=c("service_id", "trip_headsign", "direction_id","route_id", "period")]
# 
# # Get Headway in Minutes
# mostCommonStop_Sch[, headwayMinutes := periodMinutes/tripCount]
# 
# test3 <- mostCommonStop_Sch[route_id == '3', .(period, direction_id, service_id, trip_headsign, tripCount, headwayMinutes)] %>% distinct()
# test3_2 <- test3[service_id == 'JUN21-MVS-BUS-Weekday-01']
# please <- test3_2[, .(meanHeadway = round(mean(headwayMinutes))), by = .(period, direction_id)]%>%distinct
# 
# # SUMMARY Headway by route
# headwayByServiceId <- mostCommonStop_Sch[, .(meanHeadway = round(mean(headwayMinutes))), by = .(route_id, period, direction_id, service_id, trip_headsign)] %>% distinct()  
# headwayByAll <- mostCommonStop_Sch[, .(meanHeadway = round(mean(headwayMinutes))), by = .(route_id, period, direction_id)] %>% distinct()  
# 
# # Order Output Tables by TOD and Direction
# setorderv(headwayByServiceId, c("trip_headsign", "period", "direction_id"))
# setorderv(headwayByAll, c("period", "direction_id"))
# 
# # Get for A Line
# aLineByServiceId <- headwayByServiceId[route_id == '921']%>%distinct()
# aLineByAll <- headwayByAll[route_id == '3']%>%distinct()
# 
# 
# 
# 
# 
# 
# # Get all service on week-day 1
# # Includes these service_ids:
# # "AUG21-MVS-MTS-Weekday-01" "AUG21-MVS-BUS-Weekday-01" "AUG21-MVS-NS-Weekday-01"  "AUG21-MVS-UM-Weekday-01" 
# # "AUG21-MVS-SUB-Weekday-01" "AUG21-RAIL-Weekday-01" 
# 
# 
# # Find unique service_ids (include all to average over)
# uniqueServiceIds <- c("ALL", unique(sch$service_id))
# print(paste(length(uniqueServiceIds)-1, "Unique Service Ids"))
# 
# singleSched <- sch[sch$service_id == uniqueServiceIds[2], ]%>%distinct()
# 
# # Get all routes operating on selected service
# routeList <- gtools::mixedsort(unique(singleSched$route_id))
# 
# unique(singleSched[direction_id == 0]$direction)
# # Subset Single Schedule By Route_id
# 
# 
# # FIX SERIVCE ID SELECT
# selectedRouteTable <- sch[route_id == "3" & service_id == uniqueServiceIds[2]]
#   
#   # Get Headsigns associated with Direction
#   dir0_Headsign <- unique(selectedRouteTable[direction_id == 0]$trip_headsign)
#   dir1_Headsign <- unique(selectedRouteTable[direction_id == 1]$trip_headsign)
#   print(paste('Dir0', dir0_Headsign))
#   print(paste('Dir1', dir1_Headsign))
#   
#   
#   selectedRouteTable[, tripCountPerPeriodDir:=.N, by=c("direction_id","date")]
#   
#   
#   freqs <- sch[stop_sequence == 1, .(trip_id,
#                                      start_time = departure_time,
#                                      end_time = shift(departure_time, 1, type = 'lead'),
#                                      secs = departure_time,
#                                      headway_secs = c(diff(unclass(departure_time)), NA),
#                                      headway_prev = c(NA, diff(unclass(departure_time))), exact_times = 0L
#   ),
#   keyby = c('service_id', 'route_id', 'direction_id')]
#   
#   
#   
#   
#   # Set Time Period Times
#   morningStart <- "06:00:00"
#   morningEnd <- "09:00:00"
#   middayStart <- morningEnd
#   middayEnd <- "15:00:00"
#   pmPeakStart <- middayEnd
#   pmPeakEnd <- "18:30:00"
#   
#   # Subset Route Data into Time Periods
#   selectedRouteTable_MorningPeak <- selectedRouteTable %>% dplyr::filter(arrival_time >= as.integer(gtfsFunctions::as.TransitTime(as.character(morningStart))) & arrival_time <= as.integer(gtfsFunctions::as.TransitTime(as.character(morningEnd))))
#   selectedRouteTable_Midday <- selectedRouteTable %>% dplyr::filter(arrival_time >= as.integer(gtfsFunctions::as.TransitTime(as.character(middayStart))) & arrival_time <= as.integer(gtfsFunctions::as.TransitTime(as.character(middayEnd))))
#   selectedRouteTable_PMPeak <- selectedRouteTable %>% dplyr::filter(arrival_time >= as.integer(gtfsFunctions::as.TransitTime(as.character(pmPeakStart))) & arrival_time <= as.integer(gtfsFunctions::as.TransitTime(as.character(pmPeakEnd))))
#   
#   # Get Duration of Time Periods
#   morningPeakTimeDiff_Mins <- as.numeric((gtfsFunctions::as.TransitTime(as.character(morningEnd))- gtfsFunctions::as.TransitTime(as.character(morningStart)))/60)
#   middayTimeDiff_Mins <- as.numeric((gtfsFunctions::as.TransitTime(as.character(middayEnd))- gtfsFunctions::as.TransitTime(as.character(middayStart)))/60)
#   pmPeakTimeDiff_Mins <- as.numeric((gtfsFunctions::as.TransitTime(as.character(pmPeakEnd))- gtfsFunctions::as.TransitTime(as.character(pmPeakStart)))/60)
# 
# 
# # Get Count of Buses Per Hour Per Direction
# # Morning
# selectedRouteTable_MorningFreq_Dir_0 <- morningPeakTimeDiff_Mins/length(unique(selectedRouteTable_MorningPeak[direction_id == 0]$trip_id))
# selectedRouteTable_MorningFreq_Dir_1 <- morningPeakTimeDiff_Mins/length(unique(selectedRouteTable_MorningPeak[direction_id == 1]$trip_id))
# print(paste("AM Peak Dir. 0: ", round(selectedRouteTable_MorningFreq_Dir_0,1),
#             "AM Peak Dir. 1: ", round(selectedRouteTable_MorningFreq_Dir_1,1)))
# 
# #Midday
# selectedRouteTable_MiddayFreq_Dir_0 <- middayTimeDiff_Mins/length(unique(selectedRouteTable_Midday[direction_id == 0]$trip_id))
# selectedRouteTable_MiddayFreq_Dir_1 <- middayTimeDiff_Mins/length(unique(selectedRouteTable_Midday[direction_id == 1]$trip_id))
# print(paste("Midday Dir. 0: ", round(selectedRouteTable_MiddayFreq_Dir_0,1),
#             "Midday Dir. 1: ", round(selectedRouteTable_MiddayFreq_Dir_1,1)))
# 
# #PM Peak
# selectedRouteTable_PMPeakFreq_Dir_0 <- pmPeakTimeDiff_Mins/length(unique(selectedRouteTable_PMPeak[direction_id == 0]$trip_id))
# selectedRouteTable_PMPeakFreq_Dir_1 <- pmPeakTimeDiff_Mins/length(unique(selectedRouteTable_PMPeak[direction_id == 1]$trip_id))
# print(paste("PM Peak Dir. 0: ", round(selectedRouteTable_PMPeakFreq_Dir_0,1),
#             "PM Peak Dir. 1: ", round(selectedRouteTable_PMPeakFreq_Dir_1,1)))
# 
# # Get Line Length Stats
# avg_lineLengthMiles <- mean(selectedRouteTable[stop_sequence == max(selectedRouteTable$stop_sequence)]$shape_dist_traveled)/1609.34
# print(avg_lineLengthMiles)
# summary_lineLengthMiles <- summary(selectedRouteTable[stop_sequence == max(selectedRouteTable$stop_sequence)]$shape_dist_traveled)/1609.34
# print(summary_lineLengthMiles)
# print('Diff in Max to Min')
# print(summary_lineLengthMiles["Max."]-summary_lineLengthMiles["Min."])
# 
# 
# 



