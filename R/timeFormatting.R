#' Function to convert HH:MM:SS to seconds after midnight
#'
#' @param x character column or single character of times in "HH:MM:SS" format.
#'
#' @return character column or single character in data.table ITime format which displays regular time but is made of integer seconds
#' @export
#' 
as.TransitTime <- function(x) {
  V1 = V2 = V3 = NULL # Fix data.table NSE CRAN checks
  y = data.table::as.data.table(data.table::tstrsplit(x, ':', type.convert = TRUE, fill = 0L))
  if (!'V3' %in% names(y)) {y[, `:=` (V3 = 0L)]}
  y[, structure(V1 * 3600 + V2 * 60 + V3, class = 'ITime')]
}

#' Function to convert seconds after midnight to HH:MM:SS
#'
#' @param x character column or single character of times in integer or ITime format.
#'
#' @return character column or single character in character "HH:MM:SS" format
#' @export
#' 
transitTime2HHMMSS <- function(x){
  paste(stringr::str_pad(x%/%3600, 2, pad = "0"),
        stringr::str_pad(x%%3600%/%60, 2, pad = "0"),
        stringr::str_pad(x%%60, 2, pad = "0"),
        sep = ':')
}



#' Function to create time of day (TOD) data.table
#'
#' @param earlyStart integer seconds past midnight at which the early period starts
#' @param AMPeakStart integer seconds past midnight at which the AMPeak period starts
#' @param MiddayStart integer seconds past midnight at which the midday period starts
#' @param PMPeakStart integer seconds past midnight at which the PMPeak period starts
#' @param EveningStart integer seconds past midnight at which the evening period starts
#' @param NightStart integer seconds past midnight at which the night period starts
#' @param NightEnd integer seconds past midnight at which the night period starts
#'
#' @return character column or single character in character "HH:MM:SS" format
#' @export
#' 
# Add Time of Day By Stop_time
# Reference table of seconds after midnight time of day (TOD) table for reference
todTable <- function(earlyStart   = 14400,
                     AMPeakStart  = 21600,
                     MiddayStart  = 32400,
                     PMPeakStart  = 54000,
                     EveningStart = 66600,
                     NightStart   = 75600,
                     NightEnd     = 86400){
  TOD <- data.table::data.table(BeginTime = c(0,earlyStart, AMPeakStart, MiddayStart, PMPeakStart, EveningStart, NightStart, NightEnd),
                                EndTime = c(earlyStart, AMPeakStart, MiddayStart, PMPeakStart, EveningStart, NightStart, NightEnd, NightEnd+earlyStart),
                                Period = factor(c("Owl","Early","AM Peak","Midday",
                                                  "PM Peak","Evening", "Night","Owl"),
                                                levels = c("Early","AM Peak","Midday", "PM Peak",
                                                           "Evening", "Night", "Owl"),
                                                ordered = T))
  
  # Create Time Span Column
  TOD[, timeSpan := paste0(gtfsFunctions::transitTime2HHMMSS(BeginTime),
                           '--',
                           gtfsFunctions::transitTime2HHMMSS(EndTime))]
  # Create Minutes Per TOD
  TOD[, periodMinutes := (EndTime - BeginTime)/60]
  
  TOD
}

