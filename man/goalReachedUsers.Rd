\name{Glib.getGoalReachedUsers}
\alias{Glib.getGoalReachedUsers}
\title{Glib.getGoalReachedUsers}
\usage{
Glib.getGoalReachedUsers(manager)
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
}
\description{
Return the goal reached users (vector).
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfiles(UserProfilesManager)
goalReachedUsers <- Glib.getGoalReachedUsers(UserProfilesManager)
}
}
