\name{Glib.setDirs}
\alias{Glib.setDirs}
\title{Glib.setDirs}
\usage{
Glib.setDirs(env, input, output)
}
\arguments{
  \item{env}{Glib env object.}
  \item{input}{input dir full path.}
  \item{output}{output dir full path.}
}
\description{
If you set the dir pathes, you can use relative path other Glib functions.
}
\examples{
\dontrun{
Glib.setDirs(GlibEnv, '~/my-r-project/inputs', '~/my-r-project/outputs')
}
}
