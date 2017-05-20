\name{Glib.createPartition}
\alias{Glib.createPartition}
\title{Glib.createPartition}
\usage{
Glib.createPartition(manager, rate = c(50, 50))
}
\arguments{
  \item{manager}{Glib user profiles manager object.}
  \item{rate}{the rates (vector) of partition.}
}
\description{
 Use it to create training- and test data partition.
}
\examples{
\dontrun{
UserProfilesManager <- Glib.createUserProfilesManager(GlibEnv, data)
Glib.createUserProfiles(UserProfilesManager)
Glib.createPartition(UserProfilesManager, c(50, 50))
userProfiles <- Glib.getUserProfiles(UserProfilesManager)
trainingUserProfiles <- userProfiles[userProfiles[,'part'] == 1,]
testUserProfiles <- userProfiles[userProfiles[,'part'] == 2,]
}
}
