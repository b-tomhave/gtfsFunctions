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
