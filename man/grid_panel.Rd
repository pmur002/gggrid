\name{grid_panel}
\alias{grid_panel}
\alias{grid_group}
\title{
  Add a \pkg{grid} Layer.
}
\description{
  Adds a layer to a \pkg{ggplot2} plot based on a raw \pkg{grid} grob
  or on a function that generates a 'grid' grob.
}
\usage{
grid_panel(grob = nullGrob(),
           mapping = NULL, data = NULL, stat = "identity",
           position = "identity", inherit.aes = TRUE, debug = NULL, ...)
grid_group(grob = nullGrob(),
           mapping = NULL, data = NULL, stat = "identity",
           position = "identity", inherit.aes = TRUE, debug = NULL, ...)
}
\arguments{
  \item{grob}{
    Either a \pkg{grid} grob or a function.  The function must accept
    two arguments (\code{data} and \code{coords}) and must return
    a \pkg{grid} grob.
}
  \item{mapping, data, stat, position, inherit.aes}{
    Standard arguments to a \pkg{ggplot2} Geom.
  }
  \item{debug}{
    Either \code{NULL} or a function.  The function must accept
    two arguments (\code{data} and \code{coords}) and will usually
    just print debugging output of some kind.
  }
  \item{\dots}{
    Other arguments passed on to \code{ggplot2::layer()}.
  }
}
\details{
  The \code{grid_panel()} function will draw the \code{grob}, or call the
  \code{grob} function, once for each \pkg{ggplot2} panel.

  The \code{grid_group()} function will draw the \code{grob}, or call the
  \code{grob} function, once for each \pkg{ggplot2} group.
}
\author{
  Paul Murrell
}
\examples{
tg <- textGrob("label",
               x=unit(1, "npc") - unit(2, "mm"),
               y=unit(1, "npc") - unit(2, "mm"),
               just=c("right", "top"))
ggplot(mtcars) +
    geom_point(aes(disp, mpg)) +
    grid_panel(tg)
}
\keyword{ dplot }
