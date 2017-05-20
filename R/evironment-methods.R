Glib.createEnvironment <- function() GlibEnvironment()

Glib.setDirs <- function(env, input, output) UseMethod("Glib.setDirs")
Glib.setDirs.GlibEnvironment <- function(env, input, output) env$setDirs(input, output)

Glib.setConfig <- function(env, key, value) UseMethod("Glib.setConfig")
Glib.setConfig.GlibEnvironment <- function(env, key, value) env$setConfig(key, value)

Glib.getConfig <- function(env, key) UseMethod("Glib.getConfig")
Glib.getConfig.GlibEnvironment <- function(env, key) return(env$getConfig(key))

Glib.getConfigList <- function(env) UseMethod("Glib.getConfigList")
Glib.getConfigList.GlibEnvironment <- function(env) return(env$getConfigList())
