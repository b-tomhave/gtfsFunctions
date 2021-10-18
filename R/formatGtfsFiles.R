#' Format GTFS zip file into Data.Table Objects with a routes.txt field formattedRouteName that is the route_short_name (or route_id if no short_name)
#'
#' This function takes a file path to a gtfs Zip file converts all the primary required files (agency, stops, routes, trips,
#' stop_times, calendar, calendar_dates, shapes) into data.table objects. URL links are formatted as HTML links for shiny rendering
#' and dashes are removed in route_id's if the same after dash value occurs more than 10 times (to fix transitfeeds duplication of route_ids),
#' A new field in the routes.txt file is created (formattedRouteName) that is the route_short_name (or route_id if no short_name). Useful for working with data where route_id doesn't directly correspond to same number that is public-facing
#'
#' @param gtfsZipPath character file path to gtfs zip file
#' @return A gtfs/list object with a single data.table for each of the primary gtfs tables (listed above)
#' @import data.table
#' @export
#' 
formatGTFSObject <- function(gtfsZipPath){
  # Import tidytransit gtfs object from path and only focusing on key required files listed below
  #ptm <- proc.time()
  possibleGTFSFiles <- c("agency.txt",
                         "stops.txt",
                         "routes.txt",
                         "trips.txt",
                         "stop_times.txt",
                         "calendar.txt",
                         "calendar_dates.txt",
                         "fare_attributes.txt",
                         "fare_rules.txt",
                         "shapes.txt",
                         "frequencies.txt",
                         "transfers.txt",
                         "pathways.txt",
                         "levels.txt",
                         "feed_info.txt",
                         "translations.txt",
                         "attributions.txt")
  
  zipFileNames <- unzip(zipfile = gtfsZipPath)
  #print(proc.time() - ptm) # Two Seconds to get to here
  # x <- gtfsio::import_gtfs(as.character(gtfsZipPath),
  #                          skip = basename(excludeNonTxtAndNested))
  # 
  # print(stringr::str_remove_all(zonator::file_path_sans_ext(zipFileNames)[(basename(zipFileNames) %in% possibleGTFSFiles)],
  #                               "\\./"))
  # # zipFileNames[!(zipFileNames%in%excludeNonTxtAndNested)]
  x <- gtfsio::import_gtfs(as.character(gtfsZipPath),
                            files = stringr::str_remove_all(zonator::file_path_sans_ext(zipFileNames)[(basename(zipFileNames) %in% possibleGTFSFiles)],
                                                              "\\./"))
  #print(proc.time() - ptm) # This takes 6 seconds
  # Ensure all Input Files are Data.Table Objects
  setDT(x$stop_times)
  setDT(x$trips)
  setDT(x$shapes)
  # setDT(x$calendar)
  # setDT(x$calendar_dates)
  setDT(x$stops)
  setDT(x$agency)
  setDT(x$routes)
  #print(proc.time() - ptm)
  # Only Include Stops that occur in stop_times file
  x$stops <- x$stops[stop_id %in% unique(as.character(x$stop_times$stop_id))]
  
  # Only Include routes that occur in trips file
  x$routes <- x$routes[route_id %in% unique(as.character(x$trips$route_id))]
  
  # Ensure URL's are Actually Clickable (i.e. format as HTML)
  x$routes$route_url <- paste0("<a href='", x$routes$route_url, "'>", x$routes$route_url,"</a>")
  x$stops$stop_url <- paste0("<a href='", x$stops$stop_url, "'>", x$stops$stop_url,"</a>")
  x$agency$agency_url <- paste0("<a href='", x$agency$agency_url, "'>", x$agency$agency_url,"</a>")
  
  
  # ROUTES.txt: Change column type and row order
  x$routes <- x$routes[gtools::mixedorder(as.character(route_short_name))] # Set row order based on route_short_name
  x$routes$route_type <- as.factor(x$routes$route_type) # Make route_type a factor for easier datatable filtering
  
  # Only change agency_id class if present (i.e. STL Metro Transit Doesn't Have Field)
  if ("agency_id" %in% names(x$routes)){
    x$routes$agency_id <- as.factor(x$routes$agency_id) # Make agency_id a factor for easier datatable filtering 
  }
  
  
  x$trips <- unique(x$trips)
  x$routes <- unique(x$routes)
  
  # Create Column That is route_short_name if present, else route_id
  # Useful for working with data where route_id doesn't directly correspond to same number that is public-facing
  x$routes$formattedRouteName <- with(x$routes, ifelse(route_short_name == "", as.character(route_id), as.character(route_short_name)))
  #print(proc.time() - ptm)
  return(x)
}
#test <- formatGTFSObject("/Users/bentomhave/Documents/Data_GTFS/Summer2021/CTA_June21 2.zip")
