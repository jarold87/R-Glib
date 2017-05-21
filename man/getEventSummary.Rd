\name{Glib.getEventSummary}
\alias{Glib.getEventSummary}
\title{Glib.getEventSummary}
\usage{
Glib.getEventSummary(manager, event)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
  \item{event}{event name.}
}
\description{
Return the summary of event.
}
\examples{
\dontrun{
clickSummary <- Glib.getEventSummary(UserProfilesManager, 'click')
}
}
