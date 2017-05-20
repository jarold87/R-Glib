GlibUserProfiles <- function(GlibEnvironment, data) {

  thisEnv <- environment()

  userProfileStat <- NULL
  userProfiles <- NULL

  this <- list(

    thisEnv = thisEnv,

    test = function() {
      return('UP: OK')
    },

    createUserProfiles = function() {
      events <- getConfig('events')
      goalEvent <- getConfig('goalEvent')
      if (is.null(data) == TRUE) {
        stop(paste("Glib UserProfiles: ", "Data is NULL!", sep=""))
      }
      if (length(events) < 1) {
        stop(paste("Glib UserProfiles: ", "Events are missing!", sep=""))
      }
      ret <- createPrepare(data, events)
      assign("userProfileStat", ret[['stat']], thisEnv)
      assign("userProfiles", createProfiles(data, events, ret[['dataList']], goalEvent), thisEnv)
    },

    getUserProfiles = function() {
      return(get("userProfiles", thisEnv))
    },

    getStat = function() {
      return(get("userProfileStat", thisEnv))
    },

    getGoalReachedUsers = function() {
      goalEvent <- getConfig('goalEvent')
      if (is.null(goalEvent) == FALSE) {
        return(getGoalReachedUsers(data, goalEvent))
      }
      stop(paste("Glib UserProfiles: ", "Conversion event is missing!", sep=""))
    },

    createPartition = function(rate, factor = 'goal') {
      d <- get("userProfiles", thisEnv)
      if (sum(rate) != 100) {
        stop(paste("Glib: ", "Invalid rate!", sep=""))
      }
      r <- tapply(rownames(d), list(d[,factor]), function(x) { x })
      l <- lapply(r, length)
      m <- min(unlist(l))
      ac <- lapply(r, function(x) { sample(x, m) })
      parts <- lapply(rate, function(x) { floor((m / (100 / x))) })
      pc <- list()
      partValues <- c()
      partNames <- c()
      for (i in 1:length(parts)) {
        if (i > 1) { f <- t + 1; t <- parts[[i]] + t } else { f <- 1; t <- parts[[i]] }
        r <- unlist(lapply(ac, function(x) { x[f:t] }))
        names(r) <- NULL
        partValues <- c(partValues, rep(i, length(r)))
        partNames <- c(partNames, r)
      }
      d <- cbind(d, part = rep(0, nrow(d)))
      d[partNames,'part'] <- partValues
      d <- d[d[,'part'] > 0,]
      # Test
      cpn <- c()
      for (i in 1:length(parts)) {
        cpn <- c(cpn, nrow(d[d[,'part'] == i,]))
      }
      if (identical(unlist(parts) * 2, cpn)) {
        stop(paste("Glib: ", "Failed! (1)", sep=""))
      }
      if (nrow(d) != length(unique(rownames(d)))) {
        stop(paste("Glib: ", "Failed! (2)", sep=""))
      }
      assign("userProfiles", d, thisEnv)
    }

  )

  getConfig <- function(key) {
    return(GlibEnvironment$getConfig(key))
  }

  createPrepare <- function(data, events) {
    dataList <- list()
    groupedValues <- aggregate(data$user_id, list(data[,getConfig('eventColumn')]), function(x) { length(unique(x)) })
    values <- groupedValues[,2]
    stat <- matrix(calculate(values), ncol=4)
    colnames(stat) <- c('sum', 'mean', 'min', 'max')
    rownames(stat) <- c('user')
    dataList[['user']] <- groupedValues
    for (i in 1:length(events)) {
      event <- events[i]
      fData <- data[data$event == event,]
      if (nrow(fData)) {
        groupedValues <- aggregate(fData[,getConfig('eventColumn')], list(fData[,getConfig('userIdColumn')]), length)
        values <- groupedValues[,2]
        stat <- rbind(stat, calculate(values))
        stat <- addRowNameToTable(stat, event)
        dataList[[event]] <- groupedValues
      } else {
        stat <- rbind(stat, c(0, 0, 0, 0))
        stat <- addRowNameToTable(stat, event)
      }
    }
    return(list(stat=stat, dataList=dataList))
  }

  createProfiles <- function(data, events, dataList, goalEvent) {
    uc <- getConfig('userIdColumn')
    users <- unique(data[,uc])
    r <- lapply(users, function(x) {
      r <- c()
      for (i in 1:length(events)) {
        v <- dataList[[events[i]]]
        n <- 0
        if (length(c(v[v[,1] == x,2])) > 0) {
          n <- v[v[,1] == x,2]
        }
        r <- c(r, n)
      }
      return(r)
    })
    profiles <- matrix(unlist(r), nrow=length(users), ncol=length(events), byrow=T)
    rownames(profiles) <- users
    colnames(profiles) <- c(events)
    if (is.null(goalEvent) == FALSE) {
      goalReachedUsers <- getGoalReachedUsers(data, goalEvent)
      profiles <- cbind(profiles, goal = c(0))
      profiles[goalReachedUsers,'goal'] <- 1
    }
    return(profiles)
  }

  getGoalReachedUsers <- function(data, goalEvent) {
    return(unique(data[data[,getConfig('eventColumn')] == goalEvent,getConfig('userIdColumn')]))
  }

  calculate <- function(values) {
    return(c(
      round(sum(values), digits = 2),
      round(mean(values), digits = 2),
      round(min(values), digits = 2),
      round(max(values), digits = 2)
    ))
  }

  addRowNameToTable <- function(table, name) {
    rownames(table) <- c(rownames(table)[1:(length(rownames(table)) - 1)], name)
    return(table)
  }

  assign('this', this, envir=thisEnv)
  class(this) <- append(class(this), "GlibUserProfiles")
  return(this)

}
