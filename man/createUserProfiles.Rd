\name{Glib.createUserProfiles}
\alias{Glib.createUserProfiles}
\title{Glib.createUserProfiles}
\usage{
Glib.createUserProfiles(manager)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
}
\description{
Create the user profiles.
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfiles(UserProfilesManager)
}
}
