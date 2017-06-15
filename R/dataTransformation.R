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
      data <- read.table(filePath, quote='"', header=TRUE, sep=",", fill = TRUE)
      data <- prepare(data)
      assign("trData", data, thisEnv)
    },

    filterByDate = function(values = c(), unit = 'day') {
      data <- get("trData", thisEnv)
      d <- data
      df <- getConfig('dateUnitAndFormat')
      dc <- getConfig('dateColumn')
      if (!unit %in% names(df)) {
        stop(paste("Glib: ", "Unit is not exist!", " (", unit, ")", sep=""))
      }
      d[,paste(unit, 'GlibTemp', sep="_")] <- as.integer(format(as.POSIXct(d[,dc]), format=df[unit]))
      data <- data[d[,paste(unit, 'GlibTemp', sep="_")] %in% values,]
      assign("trData", data, thisEnv)
    },

    createDateColumns = function() {
      data <- get("trData", thisEnv)
      df <- getConfig('dateUnitAndFormat')
      dc <-getConfig('dateColumn')
      v <- lapply(df, function(x) {
        as.integer(format(as.POSIXct(data[,dc]), format=x))
      })
      m <- matrix(unlist(v), nrow = nrow(data), byrow = FALSE)
      data[,attributes(df)$names] <- m
      assign("trData", data, thisEnv)
    },

    normalizeColumnValues = function(column, oldValues = c(), newValue) {
      data <- get("trData", thisEnv)
      values <- data[,column]
      if (!is.factor(values)) {
        data[,column] <- values[values %in% oldValues] <- newValue
        return(data)
      }
      levels <- c(attributes(values)$levels, newValue)
      values <- as.vector(values)
      values[values %in% oldValues] <- newValue
      levels <- levels[!levels %in% oldValues]
      data[,column] <- factor(values, levels = levels)
      assign("trData", data, thisEnv)
    },

    filterByUserLifetime = function(value, unit = 'day', filterUser = TRUE) {
      data <- get("trData", thisEnv)
      d <- data
      df <- getConfig('dateUnitAndFormat')
      ds <- getConfig('dateUnitInSec')
      dc <- getConfig('dateColumn')
      uc <- getConfig('userIdColumn')
      if (!unit %in% names(df) || !unit %in% names(ds)) {
        stop(paste("Glib: ", "Unit is not exist!", " (", unit, ")", sep=""))
      }
      tc <- 'GlibTemp_timestamp'
      d[,tc] <- as.numeric(as.POSIXct(d[,dc]))
      t <- aggregate(d[,tc], list(d[,uc]), function(x) x[length(x)] - x[1] )
      filteredUsers <- as.vector(t[t[,2] <= value * ds[unit],1])
      if (filterUser) {
        d <- d[d[,uc] %in% filteredUsers,]
        assign("trData", d, thisEnv)
        return()
      }
      ci <- aggregate(d[!(d[,uc] %in% filteredUsers),tc], list(d[!(d[,uc] %in% filteredUsers),uc]), function(x) {
        f <- x[1]
        r <- unlist(lapply(c(2:length(x)), function(i) {
          if (is.na(x[i])) return(c())
          if (x[i] - f > value * ds[unit]) return(x[i])
        }))
        if (length(r)) return(as.integer(r[1]))
        return(0)
      })
      d$GlibTemp_keep <- c(TRUE)
      groups <- createGroupsByVector(ci[,1])
      ret <- mclapply(1:length(groups), function(x) {
        groupUsers <- groups[[x]]
        unlist(lapply(c(1:nrow(ci)), function(i) {
          userId <- ci[i,1]
          cutTime <- ci[i,2]
          if (cutTime > 0) rownames(d[d$user_id == userId & d[,tc] >= cutTime, ])
        }))
      }, mc.cores = length(groups))
      d[unlist(ret),'GlibTemp_keep'] <- c(FALSE)
      d <- d[d$GlibTemp_keep == TRUE,]
      d$GlibTemp_keep <- NULL
      assign("trData", d, thisEnv)
    },

    getData = function() {
      return(get("trData", thisEnv))
    }

  )

  prepare <- function(data) {
    dc <- getConfig('dateColumn')
    uc <- getConfig('userIdColumn')
    pc <- getConfig('productIdColumn')
    data <- data[which(data[,dc] != ''),]
    data[,uc] <- as.vector(as.character(data[,uc]))
    data[,pc] <- as.vector(as.character(data[,pc]))
    if (grepl('"', data[1,dc])) {
      data[,dc] <- gsub('"', "", data[,dc])
    }
    if (grepl('T', data[1,dc]) & grepl('Z', data[1,dc])) {
      data[,dc] <- as.character(as.POSIXlt(as.POSIXct(data[,dc], "%Y-%m-%dT%H:%M:%S", tz="UTC"), getConfig('defaultTimeZone')))
    }
    return(data)
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

  getConfig <- function(key) {
    return(GlibEnvironment$getConfig(key))
  }

  assign('this', this, envir=thisEnv)
  class(this) <- append(class(this), "GlibDataTransformation")
  return(this)

}
