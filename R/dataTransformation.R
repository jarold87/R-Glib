GlibDataTransformation <- function(GlibEnvironment) {

  thisEnv <- environment()

  trData <- NULL

  this <- list(

    thisEnv = thisEnv,

    init = function(data) {
      if (is.null(data) == FALSE) {
        data <- prepare(data)
      }
      assign("trData", data, thisEnv)
    },

    read = function(file, abs = FALSE) {
      filePath <- GlibEnvironment$getFilePath('inputDir', file, abs)
      data <- tryCatch( {
        fread(filePath, quote='"', header=TRUE, sep=",", fill = TRUE)
      }, warning = function(war) {
        return(FALSE)
      }, error = function(err) {
        return(FALSE)
      })
      data <- prepare(data)

      assign("trData", data, thisEnv)
      remove(data)
    },

    filterByDate = function(values = c(), unit = 'day') {
      data <- get("trData", thisEnv)
      d <- data
      df <- getConfig('dateUnitAndFormat')
      dc <- getConfig('dateColumn')
      if (!unit %in% names(df)) {
        stop(paste("Glib: ", "Unit is not exist!", " (", unit, ")", sep=""))
      }
      d[(paste(unit, 'GlibTemp', sep="_")) := as.integer(format(as.POSIXct(d[[dc]]), format=df[unit]))]
      data <- data[d[[paste(unit, 'GlibTemp', sep="_")]] %in% values,]
      assign("trData", data, thisEnv)
    },

    createDateColumns = function() {
      data <- get("trData", thisEnv)
      df <- getConfig('dateUnitAndFormat')
      dc <-getConfig('dateColumn')
      v <- lapply(df, function(x) {
        as.integer(format(as.POSIXct(data[[dc]]), format=x))
      })
      m <- matrix(unlist(v), nrow = nrow(data), byrow = FALSE)
      data[,(attributes(df)$names) := m]
      assign("trData", data, thisEnv)
    },

    normalizeColumnValues = function(column, oldValues = c(), newValue) {
      data <- get("trData", thisEnv)
      values <- data[[column]]
      if (!is.factor(values)) {
        data[,(column) := (values[values %in% oldValues] <- newValue)]
        return(data)
      }
      levels <- c(attributes(values)$levels, newValue)
      values <- as.vector(values)
      values[values %in% oldValues] <- newValue
      levels <- levels[!levels %in% oldValues]
      data[,(column) := factor(values, levels = levels)]
      assign("trData", data, thisEnv)
    },

    filterByUserLifetime = function(value, unit = 'day', filterUser = TRUE) {
      d <- get("trData", thisEnv)
      df <- getConfig('dateUnitAndFormat')
      ds <- getConfig('dateUnitInSec')
      dc <- getConfig('dateColumn')
      uc <- getConfig('userIdColumn')
      if (!unit %in% names(df) || !unit %in% names(ds)) {
        stop(paste("Glib: ", "Unit is not exist!", " (", unit, ")", sep=""))
      }
      tc <- 'GlibTemp_timestamp'
      d[,(tc) := as.numeric(as.POSIXct(d[[dc]]))]
      t <- aggregate(d[[tc]], d[,uc,with=F], function(x) x[length(x)] - x[1] )
      filteredUsers <- as.vector(t[t[,2] <= value * ds[unit],1])
      if (filterUser) {
        d <- d[d[[uc]] %in% filteredUsers,]
        assign("trData", d, thisEnv)
        return()
      }
      if (!nrow(d[!(d[[uc]] %in% filteredUsers),])) {
        assign("trData", d, thisEnv)
        return()
      }
      ci <- aggregate(d[!(d[[uc]] %in% filteredUsers),tc,with=F], d[!(d[[uc]] %in% filteredUsers),uc,with=F], function(x) {
        f <- x[1]
        r <- unlist(lapply(c(2:length(x)), function(i) {
          if (is.na(x[i])) return(c())
          if (x[i] - f > value * ds[unit]) return(x[i - 1])
        }))
        if (length(r)) return(as.integer(r[1]))
        return(0)
      })
      d <- cutByTimestamp(d, ci)
      print('foo')
      assign("trData", d, thisEnv)
    },

    dropEventLogsAfterGoalEvent = function() {
      d <- get("trData", thisEnv)
      tc <- 'GlibTemp_timestamp'
      goalEvent <- getConfig('goalEvent')
      dc <- getConfig('dateColumn')
      uc <- getConfig('userIdColumn')
      d[[,tc]] <- as.numeric(as.POSIXct(d[[,dc]]))
      goalReachedUsers <- getGoalReachedUsers(d)
      if (!length(goalReachedUsers)) return()
      goalEventTime <- lapply(goalReachedUsers, function(user) {
        return(d[[d$user_id == user & d$event == goalEvent, 'GlibTemp_timestamp']][1])
      })
      ci <- data.table(user = goalReachedUsers, goalEventTime = unlist(goalEventTime))
      d <- cutByTimestamp(d, ci)
      assign("trData", d, thisEnv)
    },

    dropEventLogsAfterAnEvent = function(event) {
      d <- get("trData", thisEnv)
      tc <- 'GlibTemp_timestamp'
      dc <- getConfig('dateColumn')
      uc <- getConfig('userIdColumn')
      d[[,tc]] <- as.numeric(as.POSIXct(d[[,dc]]))
      users <- unique(d[[,uc]])
      goalReachedUsers <- getGoalReachedUsers(d)
      if (length(goalReachedUsers)) users <- users[!(users %in% goalReachedUsers)]
      if (!length(users)) return()
      goalEventTime <- lapply(users, function(user) {
        return(d[d$user_id == user & d$event == event, 'GlibTemp_timestamp'][1])
      })
      ci <- data.table(user = users, goalEventTime = unlist(goalEventTime))
      d <- cutByTimestamp(d, ci)
      assign("trData", d, thisEnv)
    },

    getData = function() {
      return(get("trData", thisEnv))
    }

  )

  prepare <- function(data) {
    if (is.logical(data) || is.null(data) || is.null(colnames(data)) || colnames(data)[1] != 'id') return(FALSE)
    dc <- getConfig('dateColumn')
    uc <- getConfig('userIdColumn')
    pc <- getConfig('productIdColumn')
    data <- data[which(data[[dc]] != '')]
    data[,(uc) := as.vector(as.character(data[[uc]]))]
    data[,(pc) := as.vector(as.character(data[[pc]]))]
    if (grepl('"', data[[1,dc]])) {
      data[,(dc) := gsub('"', "", data[[dc]])]
    }
    if (grepl('T', data[[1,dc]]) & grepl('Z', data[[1,dc]])) {
      data[,(dc) := as.character(as.POSIXlt(as.POSIXct(data[[dc]], "%Y-%m-%dT%H:%M:%S", tz="UTC"), getConfig('defaultTimeZone')))]
    }
    return(data)
  }

  cutByTimestamp <- function(d, cutTable) {
    tc <- 'GlibTemp_timestamp'
    d$GlibTemp_keep <- TRUE
    groups <- createGroupsByVector(cutTable[,1], 2)
    ret <- mclapply(1:length(groups), function(x) {
      groupUsers <- groups[[x]]
      cutTable <- cutTable[cutTable[,1] %in% groupUsers, ]
      unlist(lapply(c(1:nrow(cutTable)), function(i) {
        userId <- cutTable[i,1]
        cutTime <- cutTable[i,2]
        if (cutTime > 0) {
          rownames(d[d$user_id == userId & d[[tc]] > cutTime, ])
        }
      }))
    }, mc.cores = length(groups))
    d[as.numeric(unlist(ret)), GlibTemp_keep := FALSE]
    d <- d[d$GlibTemp_keep == TRUE,]
    d$GlibTemp_keep <- NULL
    return(d)
  }

  createGroupsByVector <- function(vector, maxCore = NULL) {
    core <- getConfig('enabledCore')
    if (!is.null(maxCore) && core > maxCore) core <- maxCore
    groups <- lapply(c(1:core), function(g) {
      unit <- floor(length(vector) / core)
      till <- unit * g
      from <- till - unit + 1
      if (g == core) till <- length(vector)
      return(vector[from:till])
    })
    return(groups)
  }

  getGoalReachedUsers <- function(data) {
    goalEvent <- getConfig('goalEvent')
    if (is.null(goalEvent) == FALSE) {
      return(unique(data[data[[,getConfig('eventColumn')]] == goalEvent,getConfig('userIdColumn')]))
    }
    stop(paste("Glib UserProfiles: ", "Conversion event is missing!", sep=""))
  }

  getConfig <- function(key) {
    return(GlibEnvironment$getConfig(key))
  }

  assign('this', this, envir=thisEnv)
  class(this) <- append(class(this), "GlibDataTransformation")
  return(this)

}
