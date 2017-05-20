\name{Glib.setConfig}
\alias{Glib.setConfig}
\title{Glib.setConfig}
\usage{
Glib.setConfig(env, key, value)
}
\arguments{
  \item{env}{Glib env object.}
  \item{key}{config key.}
  \item{value}{config value.}
}
\description{
You can modify the Glib configs.
}
\examples{
\dontrun{
Glib.setConfig(GlibEnv, 'events', c('click', 'page_view'))
Glib.setConfig(GlibEnv, 'goalEvent', 'order')
}
}
