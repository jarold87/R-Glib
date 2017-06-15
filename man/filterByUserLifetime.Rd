\name{Glib.filterByUserLifetime}
\alias{Glib.filterByUserLifetime}
\title{Glib.filterByUserLifetime}
\usage{
Glib.filterByUserLifetime(manager, value = 1, unit = 'day', filterUser = TRUE)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
  \item{value}{the value (integer) of the filtering.}
  \item{unit}{the unit of the values.}
  \item{filterUser}{filter user or only their logs.}
}
\description{
Filter event logs by user lifetime. The lifetime is the elapsed time from the first to the last event log. The event log data frame will include only users who are in filter value.
}
\examples{
\dontrun{
Glib.filterByUserLifetime(DataTransformationManager, 24, 'hour')
}
}
