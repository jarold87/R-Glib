\name{Glib.normalizeColumnValues}
\alias{Glib.normalizeColumnValues}
\title{Glib.normalizeColumnValues}
\usage{
Glib.normalizeColumnValues(manager, column, oldValues = c(), newValue)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
  \item{column}{the subject column.}
  \item{oldValues}{the values (vector) being searched for, otherwise known as the needle.}
  \item{newValue}{the replacement value.}
}
\description{
Replace all occurrences of the search strings with the replacement string in the subject column.
}
\examples{
\dontrun{
DataTransformationManager <- Glib.createDataTransformationManager(GlibEnv)
Glib.readEventLogs(DataTransformationManager, 'sample-export.csv')
orderAlternatives <- c('0_order', '00_order', '000_order', 'AA_order', 'A_order', 'BB_order', 'B_order')
Glib.normalizeColumnValues(DataTransformationManager, 'event', orderAlternatives, 'order')
}
}
