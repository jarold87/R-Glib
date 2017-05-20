GlibEnvironment <- function() {

  thisEnv <- environment()

  configs <- list(

    defaultTimeZone = 'Europe/Budapest',
    dateUnitAndFormat = c(
      hour="%H",
      min="%M",
      sec="%S",
      day="%d",
      month="%m"
    ),
    dateColumn = 'date',
    userIdColumn = 'user_id',
    productIdColumn = 'product_id',
    eventColumn = 'event',
    events = c('click'),
    goalEvent = 'order'

  )

  this <- list(

    thisEnv = thisEnv,

    test = function() {
      return('E: OK')
    },

    setDirs = function(input, output) {
      configs <- get("configs", thisEnv)
      configs[['inputDir']] <- input
      configs[['outputDir']] <- output
      assign("configs", configs, thisEnv)
    },

    getFilePath = function(dir, file, abs) {
      if (abs == FALSE) {
        file <- paste(getConfig(dir), file, sep="/")
      }
      if (file.exists(file) == TRUE) {
        return(file)
      }
      stop(paste("Glib: ", "File is not exist!", " (", file, ")", sep=""))
    },

    setConfig = function(key, value) {
      configs <- get("configs", thisEnv)
      configs[[key]] <- value
      assign("configs", configs, thisEnv)
    },

    getConfig = function(key) {
      return(getConfig(key))
    },

    getConfigList = function() {
      return(get("configs", thisEnv))
    }

  )

  getConfig <- function(key) {
    configs <- get("configs", thisEnv)
    if (key %in% attributes(configs)$names) {
      return(configs[[key]])
    }
    return(NULL)
  }

  assign('this', this, envir=thisEnv)
  class(this) <- append(class(this), "GlibEnvironment")
  return(this)

}
