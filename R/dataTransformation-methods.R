Glib.createDataTransformationManager <- function(env, data = NULL) { c <- GlibDataTransformation(env) ; c$init(data) ; return(c) }

Glib.readEventLogs <- function(manager, file, abs = FALSE) UseMethod("Glib.readEventLogs")
Glib.readEventLogs.GlibDataTransformation <- function(manager, file, abs = FALSE) manager$read(file, abs)

Glib.getData <- function(manager) UseMethod("Glib.getData")
Glib.getData.GlibDataTransformation <- function(manager) manager$getData()

Glib.filterByDate <- function(manager, values = c(), unit = 'day') UseMethod("Glib.filterByDate")
Glib.filterByDate.GlibDataTransformation <- function(manager, values = c(), unit = 'day') manager$filterByDate(values, unit)

Glib.createDateColumns <- function(manager) UseMethod("Glib.createDateColumns")
Glib.createDateColumns.GlibDataTransformation <- function(manager) manager$createDateColumns()

Glib.normalizeColumnValues <- function(manager, column, oldValues = c(), newValue) UseMethod("Glib.normalizeColumnValues")
Glib.normalizeColumnValues.GlibDataTransformation <- function(manager, column, oldValues = c(), newValue) manager$normalizeColumnValues(column, oldValues, newValue)
