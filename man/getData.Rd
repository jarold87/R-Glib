\name{Glib.getData}
\alias{Glib.getData}
\title{Glib.getData}
\usage{
Glib.getData(manager)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
}
\description{
Return the transformed data frame.
}
\examples{
\dontrun{
DataTransformationManager <- Glib.createDataTransformationManager(GlibEnv)
Glib.readEventLogs(DataTransformationManager, 'sample-export.csv')
# ... do transformation with manager ...
data <- Glib.getData(DataTransformationManager)
}
}
