#' Calculate frequencies per stop from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips. If no service_ids provided, service with most trips used
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param by_directionId (optional) TRUE/FALSE indicating if segmenting route frequencies by direction_id (default TRUE)
#' @param by_headsign (optional) TRUE/FALSE indicating if segmenting frequencies trip_headsgign overall.
#' @param by_TOD (optional) TRUE/FALSE indicating if segmenting frequencies by time of day periods or overall. If by_TOD == T don't provide start and end HHMMSS
#' @param tod_earlyStart   (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Early period start (default 14400 (4AM))
#' @param tod_AMPeakStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod AM Peak period start (default 21600 (6AM))
#' @param tod_MiddayStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Midday period start (default 32400 (9AM))
#' @param tod_PMPeakStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod PM Peak period start (default 54000 (3PM))
#' @param tod_EveningStart (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Evening period start (default 66600 (6:30PM))
#' @param tod_NightStart   (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Night period start (default 75600 (9PM))
#' @param tod_NightEnd     (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Night period end (default 86400 (4AM (i.e. 28:00)))
#' @param startHHMMSS (optional) If by_TOD = False, specify a string HH:MM:SS (default 06:00:00 (6 am))
#' @param endHHMMSS (optional) If by_TOD = False, specify a string HH:MM:SS (default 22:00:00 (10 pm))
#' @param service_ids (optional) a string from the calendar dataframe identifying a particular service schedule. If no service_ids provided, service with most trips used
#'
#' @return tbl_df containing headways per stop
#' @import data.table
#' @export
#' 
get_stop_frequency <- function(gtfs_obj,
                               by_directionId = T,
                               by_headsign = T,
                               by_TOD = T,
                               tod_earlyStart   = 14400,
                               tod_AMPeakStart  = 21600,
                               tod_MiddayStart  = 32400,
                               tod_PMPeakStart  = 54000,
                               tod_EveningStart = 66600,
                               tod_NightStart   = 75600,
                               tod_NightEnd     = 86400,
                               startHHMMSS = "06:00:00",
                               endHHMMSS = "22:00:00",
                               service_ids = NULL
                               ) {
  
  `:=` <- data.table::`:=`
  
  #Initialize number of departures and direction_id placeholder as null
  n_deps <- direction_id <- NULL
  
  # Convert data.frame to data table
  data.table::setDT(gtfs_obj$stop_times)
  data.table::setDT(gtfs_obj$trips)
  
  # Check if trip_headsign exists. If not create headsign as last stop sequence stop name per trip
  if(!exists("trip_headsign", where = gtfs_obj$trips)) {
    
    # get the last id of the trip's stop sequence
    trip_headsigns <- gtfs_obj$stop_times %>% 
      group_by(trip_id) %>% 
      summarise(stop_id = stop_id[which.max(stop_sequence)]) %>% 
      left_join(gtfs_obj$stops, by="stop_id") %>% select(trip_id, trip_headsign = stop_name)
    
    # assign the headsign to the gtfs object 
    gtfs_obj$trips <- left_join(gtfs_obj$trips, trip_headsigns, by = "trip_id")
  }

  # Trip2Headsign Lookup Table
  trip2Headsign <- gtfs_obj$trips[, .(trip_id, trip_headsign)]
  
  # If no specified service_ids then create frequencies for all service ids
  if(is.null(service_ids)) {
    # Get number of departures per trip
    dep_per_trip <- gtfs_obj$stop_times[, .(n_deps = data.table::uniqueN(trip_id)), by =.(trip_id)]
    
    # Get number of departures per service_id
    dep_per_service_id <- gtfs_obj$trips[dep_per_trip, on = 'trip_id'][, .(n_deps = sum(n_deps)), by = .(service_id)]
    data.table::setorder(dep_per_service_id, -n_deps) # Sort desc by n_deps
    
    
    # Select service_id with greatest number of departures
    service_ids <- dep_per_service_id$service_id[1]
  }

  # Get trips on selected service_ids
  trips <- gtfs_obj$trips[service_id %in% service_ids]

  # Get stop times on selected service_id trips 
  stop_times <- na.omit(gtfs_obj$stop_times[trip_id %in% trips$trip_id])

  
  stop_times[,arrival_time := as.numeric(lubridate::hms(stop_times$arrival_time))]
  stop_times[,departure_time := as.numeric(lubridate::hms(departure_time))]
  # Change output based on if using TOD or custom time range
  if (by_TOD == T){
    # Make Sure Times Are Valid (in other words, ensure the tod values are sequentially greater than one another)
    # If times aren't valid error out otherwise continue
    if(!(tod_earlyStart < tod_AMPeakStart && tod_AMPeakStart < tod_MiddayStart && tod_MiddayStart < tod_PMPeakStart && tod_PMPeakStart < tod_EveningStart && tod_EveningStart < tod_NightStart && tod_NightStart < tod_NightEnd)){
      stop("Ensure that TOD time periods are in integer seconds since midnight and that they are sequentially greater than one another")
    }else{
    
    # Get TOD Associated with stop arrival and join to stop times
    stop_times <- stop_times[gtfsFunctions::todTable(tod_earlyStart,
                                                     tod_AMPeakStart,
                                                     tod_MiddayStart,
                                                     tod_PMPeakStart,
                                                     tod_EveningStart,
                                                     tod_NightStart,
                                                     tod_NightEnd
                                                     ),
                             `:=`(period = Period, periodMinutes = periodMinutes, timeSpan = timeSpan), on = .(arrival_time > BeginTime, departure_time <= EndTime)]
    }
  }else{
    # Get Stop Times in Custom Time Range
    stop_times <- stop_times[departure_time >= as.numeric(lubridate::hms(startHHMMSS)) & arrival_time <= as.numeric(lubridate::hms(endHHMMSS))] 
    
    # Create periodMinutes Field indicating the duration of custom period
    stop_times <- stop_times[, periodMinutes := (as.numeric(lubridate::hms(endHHMMSS)) - as.numeric(lubridate::hms(startHHMMSS)))/60]
    stop_times <- stop_times[, period := "custom"]
    stop_times <- stop_times[, timeSpan := paste0(startHHMMSS,'--',endHHMMSS)]

  }
  
  # Further filter Stop times to only be those in time period
  # Remove NAs that popup on join
  stop_times <- na.omit(unique(stop_times[trips[, c("trip_id", "route_id", "direction_id", "service_id")], on = .(trip_id)]))

  # Join Headsign to stop_times
  stop_times <- stop_times[trip2Headsign, on = .(trip_id)]
  # find number of departure per stop_id, route_id, service_id
  # If aggregated by route_id
  if(by_directionId == T) {
    # Check if Also Aggregating By Headsign
    if(by_headsign == T) { #Group by direction and headsign
      freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes, timeSpan = timeSpan),  by = .(stop_id, route_id, direction_id, service_id, period, trip_headsign)]
    }else{ # Group by direction but not headsign
      freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes, timeSpan = timeSpan, trip_headsign = "Not Generated"),  by = .(stop_id, route_id, direction_id, service_id, period)]
    }
  } else { # Group by headsign but not direction
    if(by_headsign == T) {
      freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes, timeSpan = timeSpan),  by = .(stop_id, route_id, service_id, period, trip_headsign)]
    }else{ # Group by neither direction nor headsign
      freq <- stop_times[, .(n_departures = .N, periodMinutes = periodMinutes, timeSpan = timeSpan, trip_headsign = "Not Generated"),  by = .(stop_id, route_id, service_id, period)]
      
    }
  }
  

  #calculate average headway
  freq$headwayMinutes <- round(freq$periodMinutes / freq$n_departures)
  freq %>% na.omit() %>% dplyr::distinct() 
}



#' Calculate frequencies per route from tidytransit gtfs object (This is addapted from tidytransit frequencies.R). Uses stop_times and trips. If no service_ids provided, service with most trips used
#'
#' @param gtfs_obj object from tidytransit read_gtfs()
#' @param by_directionId (optional) TRUE/FALSE indicating if segmenting route frequencies by direction_id (default TRUE)
#' @param by_headsign (optional) TRUE/FALSE indicating if segmenting frequencies trip_headsgign overall.
#' @param by_TOD (optional) TRUE/FALSE indicating if segmenting frequencies by time of day periods or overall. If by_TOD == T don't provide start and end HHMMSS
#' @param tod_earlyStart   (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Early period start (default 14400 (4AM))
#' @param tod_AMPeakStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod AM Peak period start (default 21600 (6AM))
#' @param tod_MiddayStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Midday period start (default 32400 (9AM))
#' @param tod_PMPeakStart  (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod PM Peak period start (default 54000 (3PM))
#' @param tod_EveningStart (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Evening period start (default 66600 (6:30PM))
#' @param tod_NightStart   (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Night period start (default 75600 (9PM))
#' @param tod_NightEnd     (optional) If by_TOD = False, specify an integer (seconds past midnight) for tod Night period end (default 86400 (4AM (i.e. 28:00)))
#' @param startHHMMSS (optional) If by_TOD = False, specify a string HH:MM:SS (default 06:00:00 (6 am))
#' @param endHHMMSS (optional) If by_TOD = False, specify a string HH:MM:SS (default 22:00:00 (10 pm))
#' @param service_ids (optional) a string from the calendar dataframe identifying a particular service schedule. If none provided, service with most trips used
#'
#' @return tbl_df headways per route
#' @import data.table
#' @export
#' 
get_route_frequency <- function(gtfs_obj,
                                by_directionId = T,
                                by_headsign = T,
                                by_TOD = T,
                                tod_earlyStart   = 14400,
                                tod_AMPeakStart  = 21600,
                                tod_MiddayStart  = 32400,
                                tod_PMPeakStart  = 54000,
                                tod_EveningStart = 66600,
                                tod_NightStart   = 75600,
                                tod_NightEnd     = 86400,
                                startHHMMSS = "06:00:00",
                                endHHMMSS = "22:00:00",
                                service_ids = NULL
                                ) {

  # Get Frequency of all routes at all stops they servce
  stops_frequency <- gtfsFunctions::get_stop_frequency(gtfs_obj,
                                        by_directionId,
                                        by_headsign,
                                        by_TOD,
                                        tod_earlyStart,
                                        tod_AMPeakStart,
                                        tod_MiddayStart,
                                        tod_PMPeakStart,
                                        tod_EveningStart,
                                        tod_NightStart,
                                        tod_NightEnd,
                                        startHHMMSS, 
                                        endHHMMSS,
                                        service_ids
                                        )  
  
  # For each route take average of frequencies at all stops on that route
  if (dim(stops_frequency)[[1]]!=0) {
    # If grouping by time of day then make sure to incude as a variable
    if (by_TOD == T){
      if (by_directionId == T){
        routes_frequency <- stops_frequency %>%
          dplyr::group_by(route_id, direction_id, trip_headsign, period, timeSpan) %>%
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
          dplyr::group_by(route_id, trip_headsign, period, timeSpan) %>%
          dplyr::summarise(total_departures = sum(n_departures),
                           median_headways = 
                           as.integer(round(median(headwayMinutes),0)),
                           mean_headways = 
                           as.integer(round(mean(headwayMinutes),0)),
                           st_dev_headways = 
                           round(sd(headwayMinutes),2),
                           stop_count = dplyr::n())
        }
    }else{
      if (by_directionId == T){
        routes_frequency <- stops_frequency %>%
          distinct() %>%
          dplyr::group_by(route_id, direction_id,trip_headsign, timeSpan) %>%
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
          dplyr::group_by(route_id, trip_headsign, timeSpan, period) %>%
          dplyr::summarise(total_departures = sum(n_departures),
                           median_headways = 
                           as.integer(round(median(headwayMinutes),0)),
                           mean_headways = 
                           as.integer(round(mean(headwayMinutes),0)),
                           st_dev_headways = 
                           round(sd(headwayMinutes),2),
                           stop_count = dplyr::n())
      }
    }
    # If not aggregating by headsign drop that column and get distinct as it has placeholder 'Not Generated' as created in stops_frequency
    data.table::setDT(routes_frequency) # Make sure output is data.table
    if (by_headsign == F){
      routes_frequency[, trip_headsign := NULL] %>% dplyr::distinct()
    }
    
    # Determine Frequency Category By Period
    routes_frequency[, freqClass := ifelse(median_headways <= 15, "15 Minutes or Better", "Other"), by = period]
    routes_frequency[, freqClass := ifelse(median_headways > 15 & median_headways <=20, "15-20 Minutes", freqClass), by = period]
    routes_frequency[, freqClass := ifelse(median_headways > 20 & median_headways <=30, "20-30 Minutes", freqClass), by = period]
    routes_frequency[, freqClass := ifelse(median_headways > 30 & median_headways <=60, "30-60 Minutes", freqClass), by = period]
    routes_frequency[, freqClass := ifelse(median_headways > 60, "60+ Minutes", freqClass), by = period]
    
  }else {
    warning("Failed to calculate frequency, try passing a service_id from calendar_df.")
  }
  return(routes_frequency %>% na.omit())
}




# library(dplyr)
# #
#devtools::install_github("b-tomhave/gtfsFunctions", force = TRUE) #(DON'T UPDATE DATA.TABLE)
# # test <- gtfsFunctions::todTable(earlyStart   = 14400,
# #                                 AMPeakStart  = 21600,
# #                                 MiddayStart  = 32400,
# #                                 PMPeakStart  = 54000,
# #                                 EveningStart = 66600,
# #                                 NightStart   = 75600,
# #                                 NightEnd     = 86400)
# 
# gtfsPath_Jul <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\MSP_MetroTransit_June21.zip"
# gtfsPath_Jul2 <- "C:\\Users\\ben.tomhave\\OneDrive - AECOM\\Documents\\Projects\\03_GTFS_Data\\ETS_Edmonton_July21.zip"
# 
# gtfs_obj <- gtfsFunctions::formatGTFSObject(gtfsPath_Jul)
# # # # aweseom <- tidytransit::read_gtfs(gtfsPath_Jul2)
# # # # test <- gtfsFunctions::routeIDAtStops(gtfs3)
# # # 
# # # ptm <- proc.time()
# # # # 1 Second
# # # 
# stops <-gtfsFunctions::get_stop_frequency(gtfs_obj,
#                                             by_directionId = F,
#                                             by_headsign = T,
#                                             by_TOD = T,
#                                             service_ids = NULL)
# # # 
# # # ptm <- proc.time()
# route_frequency <-   get_route_frequency(gtfs_obj,
#                                                         by_directionId = F,
#                                                         by_headsign = F,
#                                                         by_TOD = F,
#                                                         # tod_earlyStart   = 14400,
#                                                         # tod_AMPeakStart  = 22000,
#                                                         # tod_MiddayStart  = 32400,
#                                                         # tod_PMPeakStart  = 54000,
#                                                         # tod_EveningStart = 66600,
#                                                         # tod_NightStart   = 75600,
#                                                         # tod_NightEnd     = 86400,
#                                                         startHHMMSS = "06:00:00",
#                                                         endHHMMSS = "19:00:00",
#                                                         service_ids = NULL)
# # # 
# # # 
# # 
# # 
# # library(plotly)
# # test <- route_frequency2[period == "AM Peak"]
# # test$route_id <- factor(test$route_id, levels = unique(test$route_id)[order(test$median_headways, decreasing = T)])
# # test[, freq := ifelse(median_headways <= 15, "15 Minutes or Better", "Other")]
# # test[, freq := ifelse(median_headways > 15 & median_headways <=20, "15-20 Minutes", freq)]
# # test[, freq := ifelse(median_headways > 20 & median_headways <=30, "20-30 Minutes", freq)]
# # test[, freq := ifelse(median_headways > 30 & median_headways <=60, "30-60 Minutes", freq)]
# # test[, freq := ifelse(median_headways > 60, "60+ Minutes", freq)]
# # 
# # test$freq <- factor(test$freq, levels = c("15 Minutes or Better", "15-20 Minutes",
# #                                                                                 "20-30 Minutes", "30-60 Minutes", "60+ Minutes"))
# # # Combine into comma separated string
# # output <- test[, paste(route_id, collapse = ", "), by = freq] 
# # output$freq <- factor(output$freq, levels = c("15 Minutes or Better", "15-20 Minutes",
# #                                                                                 "20-30 Minutes", "30-60 Minutes", "60+ Minutes"))           
# # 
# # work <- knitr::combine_words(output$V1[1])
# # 
# # library(kableExtra)
# # 
# # row.names(output)
# # row.names(output) <- output$freq
# # output %>%
# #   kbl() %>%
# #   kable_paper("hover", full_width = F)
# # 
# # 
# # 
# # 
# # tbl <- reactable(
# #   route_frequency2,
# #   pagination = FALSE,
# #   defaultSorted = "exclusive_followers_pct",
# #   defaultColDef = colDef(headerClass = "header", align = "left"),
# #   columns = list(
# #     account = colDef(
# #       cell = function(value) {
# #         url <- paste0("https://twitter.com/", value)
# #         tags$a(href = url, target = "_blank", paste0("@", value))
# #       },
# #       width = 150
# #     ),
# #     followers = colDef(
# #       defaultSortOrder = "desc",
# #       cell = function(value) {
# #         width <- paste0(value * 100 / max(data$followers), "%")
# #         value <- format(value, big.mark = ",")
# #         value <- format(value, width = 9, justify = "right")
# #         bar <- div(
# #           class = "bar-chart",
# #           style = list(marginRight = "6px"),
# #           div(class = "bar", style = list(width = width, backgroundColor = "#3fc1c9"))
# #         )
# #         div(class = "bar-cell", span(class = "number", value), bar)
# #       }
# #     ),
# #     exclusive_followers_pct = colDef(
# #       name = "Exclusive Followers",
# #       defaultSortOrder = "desc",
# #       cell = JS("function(cellInfo) {
# #         // Format as percentage
# #         const pct = (cellInfo.value * 100).toFixed(1) + '%'
# #         // Pad single-digit numbers
# #         let value = pct.padStart(5)
# #         // Show % on first row only
# #         if (cellInfo.viewIndex > 0) {
# #           value = value.replace('%', ' ')
# #         }
# #         // Render bar chart
# #         return (
# #           '<div class=\"bar-cell\">' +
# #             '<span class=\"number\">' + value + '</span>' +
# #             '<div class=\"bar-chart\" style=\"background-color: #e1e1e1\">' +
# #               '<div class=\"bar\" style=\"width: ' + pct + '; background-color: #fc5185\"></div>' +
# #             '</div>' +
# #           '</div>'
# #         )
# #       }"),
# #       html = TRUE
# #     )
# #   ),
# #   compact = TRUE,
# #   class = "followers-tbl"
# # )
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # this2 <- test[, .(Routes = paste(route_id, collapse = ",")), by = freq] 
# # this[,'Routes' := lapply(.SD, strsplit, split = ','), .SDcols= 'Routes']
# # 
# # this$Routes
# # this[, 'Routes2' := list(gtools::mixedsort(unlist(Routes))), by =]
# # gtools::mixedsort(this$Routes)
# # 
# # 
# # stringr::str_squish
# # gtools::mixedsort(unlist(this$Routes[1]))
# # 
# # this[,Routes][str]
# # 
# # 
# # gtools::mixedsort(strsplit(this$Routes, ','))
# # 
# # how <- strsplit(this$Routes, ',')
# # 
# # how
# # 
# # unlist(strsplit(this$Routes, ','))
# # 
# # test <- as.list(el(strsplit(this$Routes, ",")))
# # 
# # 
# # gtools::mixedsort(this$Routes)
# # 
# # this$Routes <- sapply(strsplit(this$Routes, ','), function(x) 
# #   toString(gtools::mixedsort(x)))
# # 
# # 
# # gtools::mixedsort(output$V1)
# # 
# # # routes_frequency[, freqClass := ifelse(median_headways <= 15, "15 Minutes or Better", "Other"), by = period]
# # # routes_frequency[, freqClass := ifelse(median_headways > 15 & median_headways <=20, "15-20 Minutes", freqClass), by = period]
# # # routes_frequency[, freqClass := ifelse(median_headways > 20 & median_headways <=30, "20-30 Minutes", freqClass), by = period]
# # # routes_frequency[, freqClass := ifelse(median_headways > 30 & median_headways <=60, "30-60 Minutes", freqClass), by = period]
# # # routes_frequency[, freqClass := ifelse(median_headways > 60, "60+ Minutes", freqClass), by = period]
# # 
# # 
# # gtfsFunctions::routeId2routeShortName(gtfs_obj$routes)
# # route_frequency2$formatted <- as.character(please[route_frequency2$route_id])
# # 
# # route_frequency2[, .(shift(route_id)), by = period]
# # route_frequency2[, paste(route_id, collapse = ", "), by = period] 
# #                                            
# #                                            
# # plot_ly(test, x = ~median_headways, y = ~route_id, color = ~freq, type = "bar", name = ~freq)
# # # # proc.time() - ptm
# # # route_frequency2 <- route_frequency2[gtools::mixedorder(route_frequency2$route_id),]
