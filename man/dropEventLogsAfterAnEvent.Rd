\name{Glib.dropEventLogsAfterAnEvent}
\alias{Glib.dropEventLogsAfterAnEvent}
\title{Glib.dropEventLogsAfterAnEvent}
\usage{
Glib.dropEventLogsAfterAnEvent(manager, event)
}
\arguments{
  \item{manager}{Glib data transformation manager object.}
  \item{event}{Event name.}
}
\description{
Remove logs after an event at each user.
}
\examples{
\dontrun{
Glib.dropEventLogsAfterAnEvent(DataTransformationManager, 'conversion')
}
}
