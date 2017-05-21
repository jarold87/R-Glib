GlibUserProfiles <- function(GlibEnvironment, data) {

  thisEnv <- environment()

  userProfileStat <- NULL
  userProfiles <- NULL
  eventSummary <- NULL

  this <- list(

    thisEnv = thisEnv,

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
      assign("eventSummary", ret[['dataList']], thisEnv)
      assign("userProfiles", createProfiles(data, events, ret[['dataList']], goalEvent), thisEnv)
    },

    createUserProfilesGroupedByTimePeriods = function(periodsInMin = c(1), sumPeriodValues = TRUE) {
      events <- getConfig('events')
      goalEvent <- getConfig('goalEvent')
      if (is.null(data) == TRUE) {
        stop(paste("Glib UserProfiles: ", "Data is NULL!", sep=""))
      }
      if (length(events) < 1) {
        stop(paste("Glib UserProfiles: ", "Events are missing!", sep=""))
      }
      assign("userProfiles", createProfilesGroupedByTimePeriods(data, events, goalEvent, periodsInMin, sumPeriodValues), thisEnv)
    },

    getEventSummary = function(event) {
      return(get("eventSummary", thisEnv)[[event]])
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

  createPrepare <- function(data, events, userIdColumn = NULL) {
    dataList <- list()
    ec <- getConfig('eventColumn')
    uc <- userIdColumn
    if (is.null(uc)) uc <- getConfig('userIdColumn')
    groupedValues <- aggregate(data[,uc], list(data[,ec]), function(x) { length(unique(x)) })
    values <- groupedValues[,2]
    stat <- matrix(calculate(values), ncol=4)
    colnames(stat) <- c('sum', 'mean', 'min', 'max')
    rownames(stat) <- c('user')
    dataList[['user']] <- groupedValues
    groups <- createGroupsByVector(events)
    ret <- mclapply(1:length(groups), function(x) {
      groupEvents <- groups[[x]]
      r <- lapply(groupEvents, function(event) {
        fData <- data[data$event == event,]
        if (nrow(fData) < 1) return(NULL)
        groupedValues <- aggregate(fData[,ec], list(fData[,uc]), length)
        colnames(groupedValues) <- c('user', 'count')
        return(groupedValues)
      })
      names(r) <- groupEvents
      return(r)
    }, mc.cores = length(groups))
    unlistRet <- list()
    for (i in 1:length(ret)) {
      r <- ret[[i]]
      for (j in 1:length(r)) {
        event <- names(r)[j]
        groupedValues <- ret[[i]][[j]]
        if (!is.null(groupedValues)) {
          values <- groupedValues[,2]
          stat <- rbind(stat, calculate(values))
          stat <- addRowNameToTable(stat, event)
          dataList[[event]] <- groupedValues
        }
      }
    }
    return(list(stat=stat, dataList=dataList))
  }

  createProfiles <- function(data, events, dataList, goalEvent, userIdColumn = NULL) {
    id <- userIdColumn
    uc <- getConfig('userIdColumn')
    if (is.null(id)) id <- uc
    users <- unique(data[,id])
    groups <- createGroupsByVector(users)
    ret <- mclapply(1:length(groups), function(x) {
      groupUsers <- groups[[x]]
      r <- lapply(groupUsers, function(x) {
        c(x, data[data[,id] == x,uc][1], unlist(lapply(events, function(e) {
          v <- dataList[[e]]
          n <- 0
          if (length(c(v[v[,1] == x,2])) > 0) n <- v[v[,1] == x,2]
          return(n)
        })))
      })
      profiles <- as.data.frame(matrix(unlist(r), nrow=length(groupUsers), ncol=(length(events) + 2), byrow=T))
      colnames(profiles) <- c('id', 'user', events)
      rownames(profiles) <- profiles$id
      return(profiles)
    }, mc.cores = length(groups))
    allProfile <- do.call("rbind", ret)
    if (!is.null(goalEvent)) {
      goalReachedUsers <- getGoalReachedUsers(data, goalEvent)
      allProfile <- cbind(allProfile, goal = c(0))
      allProfile[allProfile$user %in% goalReachedUsers,'goal'] <- 1
    }
    return(allProfile)
  }

  createProfilesGroupedByTimePeriods <- function(data, events, goalEvent, periodsInMin, sumPeriodValues) {
    uc <- getConfig('userIdColumn')
    users <- unique(data[,uc])
    groups <- createGroupsByVector(users)
    ret <- mclapply(1:length(groups), function(x) {
      groupUsers <- groups[[x]]
      td <- data[data[,uc] %in% groupUsers,]
      generatedData <- generateDataByPeriods(td, periodsInMin, sumPeriodValues)
      return(generatedData)
    }, mc.cores = length(groups))
    generatedData <- do.call("rbind", ret)
    ret <- createPrepare(generatedData, events, 'user_period_id')
    assign("userProfileStat", ret[['stat']], thisEnv)
    assign("eventSummary", ret[['dataList']], thisEnv)
    ret <- mclapply(1:length(groups), function(x) {
      groupUsers <- groups[[x]]
      td <- generatedData[generatedData[,uc] %in% groupUsers,]
      profiles <- createProfiles(td, events, ret[['dataList']], NULL, 'user_period_id')
      return(profiles)
    }, mc.cores = length(groups))
    allProfile <- do.call("rbind", ret)
    userPeriodIds <- unique(generatedData[,'user_period_id'])
    allProfile[userPeriodIds,'time_period'] <- unlist(lapply(userPeriodIds, function(id) { generatedData[generatedData$user_period_id == id,'timePeriod'][1] }))
    allProfile[userPeriodIds,'is_live'] <- unlist(lapply(userPeriodIds, function(id) { generatedData[generatedData$user_period_id == id,'isLive'][1] }))
    rownames(allProfile) <- NULL
    if (!is.null(goalEvent)) {
      goalReachedUsers <- getGoalReachedUsers(data, goalEvent)
      allProfile <- cbind(allProfile, goal = c(0))
      allProfile[allProfile$user %in% goalReachedUsers,'goal'] <- 1
    }
    return(allProfile)
  }

  generateDataByPeriods <- function(data, periodsInMin, sumPeriodValues) {
    uc <- getConfig('userIdColumn')
    pc <- getConfig('productIdColumn')
    dc <- getConfig('dateColumn')
    ec <- getConfig('eventColumn')
    pac <- getConfig('pageColumn')
    goalEvent <- getConfig('goalEvent')
    names(periodsInMin) <- as.character(periodsInMin)
    firstEventOfUsers <- tapply(as.vector(data[,dc]), list(data[,uc]), function(x) { x[1] })
    generatedData <- lapply(unique(data[,uc]), function(x) {
      userId <- x
      ud <- data[data[,uc] == userId,]
      firstTime <- firstEventOfUsers[userId]
      goalEventIndex <- NULL
      if (!is.null(goalEvent)) goalEventIndex <-rownames(ud[ud[,ec] == goalEvent,])[1]
      if (!is.null(goalEventIndex) && !is.na(goalEventIndex)) ud <- ud[rownames(ud) <= goalEventIndex,]
      periods <- periodsInMin * 60 + as.POSIXlt(firstTime)
      names(periods) <- as.character(names(periodsInMin))
      logsByPeriods <- lapply(names(periods), function(p) {
        period <- periods[p]
        prevPeriod <- firstTime
        if (as.integer(p) > 1) prevPeriod <- periods[(as.integer(p) - 1)]
        d <- ud[as.POSIXlt(ud[,dc]) < period, c(uc,ec,pc,pac,dc)]
        if (sumPeriodValues == FALSE) d <- d[as.POSIXlt(d[,dc]) >= prevPeriod,]
        live <- FALSE
        if (nrow(d) < 1) return(d)
        if (nrow(ud[as.POSIXlt(ud[,dc]) >= period,]) > 0) live <- TRUE
        d[,uc] <- as.character(d[,uc])
        d[,'user_period_id'] <- paste(d[,uc], p, sep="_")
        d[,ec] <- as.vector(d[,ec])
        d[,pc] <- as.vector(d[,pc])
        d[,pac] <- as.vector(d[,pac])
        d[,dc] <- as.vector(d[,dc])
        d[,'isLive'] <- c(live)
        d[,'timePeriod'] <- as.integer(p)
        return(d)
      })
      logsByPeriodsFrame <- do.call("rbind", logsByPeriods)
      return(logsByPeriodsFrame)
    })
    return(do.call("rbind", generatedData))
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

  createGroupsByVector <- function(vector) {
    core <- getConfig('enabledCore')
    groups <- lapply(c(1:core), function(g) {
      unit <- floor(length(vector) / core)
      till <- unit * g
      from <- till - unit + 1
      if (g == core) till <- length(vector)
      return(vector[from:till])
    })
    return(groups)
  }

  assign('this', this, envir=thisEnv)
  class(this) <- append(class(this), "GlibUserProfiles")
  return(this)

}
