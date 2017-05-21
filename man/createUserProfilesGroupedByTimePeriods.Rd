\name{Glib.createUserProfilesGroupedByTimePeriods}
\alias{Glib.createUserProfilesGroupedByTimePeriods}
\title{Glib.createUserProfilesGroupedByTimePeriods}
\usage{
Glib.createUserProfilesGroupedByTimePeriods(
  manager, periodsInMin = c(1), sumPeriodValues = TRUE
)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
  \item{periodsInMin}{periods (vector) in minute.}
  \item{sumPeriodValues}{sum the value from period to period, or use absolute value.}
}
\description{
Create the user profiles grouped by definied time periods.
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfilesGroupedByTimePeriods(UserProfilesManager, c(1:5), FALSE)
}
}
