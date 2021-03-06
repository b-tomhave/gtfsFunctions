% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/routesAtStop.R
\name{routeIDAtStopsWithDirAndSeq}
\alias{routeIDAtStopsWithDirAndSeq}
\title{Function To Load GTFS Stops With Necessary Info (including direction_id and stop_sequence)}
\usage{
routeIDAtStopsWithDirAndSeq(gtfs)
}
\arguments{
\item{gtfs}{gtfs object from tidytransit read_gtfs()}
}
\value{
data.table with one stop record for each route at a stop
}
\description{
Function To Load GTFS Stops With Necessary Info (including direction_id and stop_sequence)
}
