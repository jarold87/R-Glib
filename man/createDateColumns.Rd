\name{Glib.createDateColumns}
\alias{Glib.createDateColumns}
\title{Glib.createDateColumns}
\usage{
Glib.createDateColumns(manager)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
}
\description{
Create hour, min, sec, day, month columns from date values.
}
\examples{
\dontrun{
DataTransformationManager <- Glib.createDataTransformationManager(GlibEnv)
Glib.readEventLogs(DataTransformationManager, 'sample-export.csv')
Glib.createDateColumns(DataTransformationManager)
}
}
