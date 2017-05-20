Glib.createUserProfilesManager <- function(env, data) GlibUserProfiles(env, data)

Glib.createUserProfiles <- function(manager) UseMethod("Glib.createUserProfiles")
Glib.createUserProfiles.GlibUserProfiles <- function(manager) manager$createUserProfiles()

Glib.getUserProfiles <- function(manager) UseMethod("Glib.getUserProfiles")
Glib.getUserProfiles.GlibUserProfiles <- function(manager) manager$getUserProfiles()

Glib.getStat <- function(manager) UseMethod("Glib.getStat")
Glib.getStat.GlibUserProfiles <- function(manager) manager$getStat()

Glib.getGoalReachedUsers <- function(manager) UseMethod("Glib.getGoalReachedUsers")
Glib.getGoalReachedUsers.GlibUserProfiles <- function(manager) manager$getGoalReachedUsers()

Glib.createPartition <- function(manager, rate = c(50, 50)) UseMethod("Glib.createPartition")
Glib.createPartition.GlibUserProfiles <- function(manager, rate = c(50, 50)) manager$createPartition(rate)
