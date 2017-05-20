\name{Glib.getUserProfiles}
\alias{Glib.getUserProfiles}
\title{Glib.getUserProfiles}
\usage{
Glib.getUserProfiles(manager)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
}
\description{
Return the user profiles matrix.
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfiles(UserProfilesManager)
userProfiles <- Glib.getUserProfiles(UserProfilesManager)
}
}
