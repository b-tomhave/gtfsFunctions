% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/routesAtStop.R
\name{routeId2routeShortName}
\alias{routeId2routeShortName}
\title{Function to create named list for lookup that converts route_id name into formattedRouteShortName as the value}
\usage{
routeId2routeShortName(gtfsRoutesDataTable)
}
\arguments{
\item{gtfsRoutesDataTable}{gtfs routes.txt file in data.table format}
}
\value{
namedList
}
\description{
The new field formattedRouteName is the route_short_name (or route_id if no short_name). Useful for working with data where route_id doesn't directly correspond to same number that is public-facing
}
