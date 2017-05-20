\name{Glib.getStat}
\alias{Glib.getStat}
\title{Glib.getStat}
\usage{
Glib.getStat(manager)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
}
\description{
Return an user statistics table.
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfiles(UserProfilesManager)
profileStat <- Glib.getStat(UserProfilesManager)
}
}
