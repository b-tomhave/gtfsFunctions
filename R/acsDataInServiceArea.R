#' Return vectors of state and county FIPS codes in service area
#'
#' @param gtfs object from gtfsfunctions::formatGTFSObject()
#'
#' @return list of stateFIPS and countyFIPS intersecting gtfs$stops bbox 
#' @export
#' 
getACSIntersectionVectors <- function(gtfs) {
  
  # Define piping function
  `%>%` <- magrittr::`%>%`
  
  # Get BBOX of GTFS Stops
  spatialStops <- sf::st_as_sf(gtfs$stops, coords = c("stop_lon", "stop_lat"))
  stopsBBOX    <- sf::st_bbox(sf::st_as_sf(spatialStops))
  
  # Convert stops bbox to spatial polygon in crs = 4326
  bboxPolygon <- cbind(c(stopsBBOX$xmin,stopsBBOX$xmax,stopsBBOX$xmax,stopsBBOX$xmin,stopsBBOX$xmin),
                       c(stopsBBOX$ymin,stopsBBOX$ymin,stopsBBOX$ymax,stopsBBOX$ymax,stopsBBOX$ymin)) %>%
    sp::Polygon() %>% list() %>%
    sp::Polygons(ID = 'ID1') %>% list() %>%
    sp::SpatialPolygons(proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # Get All US Counties Spatial File
  counties <- tigris::counties(cb = TRUE, year = 2016, resolution = '20m')%>%sf::st_transform(crs = 4326) # Use least detailed geography
  
  # Get counties/states that fall within stops BBOX
  countyStateIntersection <- sf::st_intersection(sf::st_as_sf(bboxPolygon),
                                                 sf::st_as_sf(counties))
  
  # Get Unique FIPS codes for state and counties
  stateFIPS  <- unique(countyStateIntersection$STATEFP)
  countyFIPS <- unique(countyStateIntersection$COUNTYFP)
  
  return(list(stateFIPS  = stateFIPS,
              countyFIPS = countyFIPS))
}



#' Return spatial polygon acs data within transit service area (defined by stops.txt bbox)
#'
#' @param gtfs object from gtfsfunctions::formatGTFSObject()
#' @param variables Character string or vector of character strings of variable IDs. tidycensus automatically returns the estimate and the margin of error associated with the variable. 
#' @param geography String string for tidycensus:;get_acs() (i.e. "tract", or "county") default is tract
#' @param year int  The year, or endyear, of the ACS sample. 5-year ACS data is available from 2009 through 2019. 1-year ACS data is available from 2005 through 2019. Defaults to 2016
#' @param survey string for the ACS contains one-year, three-year, and five-year surveys expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."
#'
#' @return sf polygon of requested acs data in transit area
#' @export
#' 
getServiceAreaACS <- function(gtfs, variables, geography = 'tract', year = 2016, survey = "acs5") {

  # Define piping function
  `%>%` <- magrittr::`%>%`
  
  intersectingFIPS <- gtfsFunctions::getACSIntersectionVectors(gtfs)
  
  # Get ACS Data
  tidycensus::get_acs(geography = geography,
                      variables = variables,
                      state     = intersectingFIPS$stateFIPS,
                      county    = intersectingFIPS$countyFIPS,
                      year      = year,
                      survey    = survey,
                      geometry  = TRUE,
                      output    = "wide")%>%
                  sf::st_transform(crs = 4326)
  
  
  
}

