\name{Glib.filterByDate}
\alias{Glib.filterByDate}
\title{Glib.filterByDate}
\usage{
Glib.filterByDate(manager, values = c(), unit = 'day')
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
  \item{values}{the values (vector) of the filtering.}
  \item{unit}{the unit of the values.}
}
\description{
Filter event logs by date.
}
\examples{
\dontrun{
DataTransformationManager <- Glib.createDataTransformationManager(GlibEnv)
Glib.readEventLogs(DataTransformationManager, 'sample-export.csv')
Glib.filterByDate(DataTransformationManager, c(6:12), 'month')
}
}
