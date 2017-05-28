Glib.createUserProfilesManager <- function(env, data) GlibUserProfiles(env, data)

Glib.createUserProfiles <- function(manager) UseMethod("Glib.createUserProfiles")
Glib.createUserProfiles.GlibUserProfiles <- function(manager) manager$createUserProfiles()

Glib.createUserProfilesGroupedByTimePeriods <- function(manager, periodsInMin = c(1), sumPeriodValues = TRUE) UseMethod("Glib.createUserProfilesGroupedByTimePeriods")
Glib.createUserProfilesGroupedByTimePeriods.GlibUserProfiles <- function(manager, periodsInMin = c(1), sumPeriodValues = TRUE) manager$createUserProfilesGroupedByTimePeriods(periodsInMin, sumPeriodValues)

Glib.getSummaryList <- function(manager) UseMethod("Glib.getSummaryList")
Glib.getSummaryList.GlibUserProfiles <- function(manager) manager$getSummaryList()

Glib.getEventSummary <- function(manager, event) UseMethod("Glib.getEventSummary")
Glib.getEventSummary.GlibUserProfiles <- function(manager, event) manager$getEventSummary(event)

Glib.getUserProfiles <- function(manager) UseMethod("Glib.getUserProfiles")
Glib.getUserProfiles.GlibUserProfiles <- function(manager) manager$getUserProfiles()

Glib.getStat <- function(manager) UseMethod("Glib.getStat")
Glib.getStat.GlibUserProfiles <- function(manager) manager$getStat()

Glib.getGoalReachedUsers <- function(manager) UseMethod("Glib.getGoalReachedUsers")
Glib.getGoalReachedUsers.GlibUserProfiles <- function(manager) manager$getGoalReachedUsers()

Glib.createPartition <- function(manager, rate = c(50, 50)) UseMethod("Glib.createPartition")
Glib.createPartition.GlibUserProfiles <- function(manager, rate = c(50, 50)) manager$createPartition(rate)
