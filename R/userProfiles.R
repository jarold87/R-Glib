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
    ec <- getConfig('eventColumn')
    uc <- getConfig('userIdColumn')
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

  createProfiles <- function(data, events, dataList, goalEvent) {
    uc <- getConfig('userIdColumn')
    users <- unique(data[,uc])
    groups <- createGroupsByVector(users)
    ret <- mclapply(1:length(groups), function(x) {
      groupUsers <- groups[[x]]
      r <- lapply(groupUsers, function(x) {
        unlist(lapply(events, function(e) {
          v <- dataList[[e]]
          n <- 0
          if (length(c(v[v[,1] == x,2])) > 0) n <- v[v[,1] == x,2]
          return(n)
        }))
      })
      profiles <- matrix(unlist(r), nrow=length(groupUsers), ncol=length(events), byrow=T)
      rownames(profiles) <- groupUsers
      colnames(profiles) <- c(events)
      return(profiles)
    }, mc.cores = length(groups))
    allProfile <- do.call("rbind", ret)
    if (is.null(goalEvent) == FALSE) {
      goalReachedUsers <- getGoalReachedUsers(data, goalEvent)
      allProfile <- cbind(allProfile, goal = c(0))
      allProfile[goalReachedUsers,'goal'] <- 1
    }
    return(allProfile)
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
