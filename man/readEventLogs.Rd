\name{Glib.readEventLogs}
\alias{Glib.readEventLogs}
\title{Glib.readEventLogs}
\usage{
Glib.readEventLogs(manager, file, abs = FALSE)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
  \item{file}{the relative or absolute path of the input file.}
  \item{abs}{is it absolute file path.}
}
\description{
Load event logs from csv file.
}
\examples{
\dontrun{
DataTransformationManager <- Glib.createDataTransformationManager(GlibEnv)
Glib.readEventLogs(DataTransformationManager, 'sample-export.csv')
}
}
