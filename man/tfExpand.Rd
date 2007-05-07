\name{tfExpand}
\alias{tfExpand}
\alias{tfExpand.default}
\alias{tfExpand.tframe}
\alias{tfTruncate}
\alias{tfTruncate.default}
\alias{tfTruncate.tframe}

\title{Expand a Tframe or Tframed Object.}
\description{Expand a tframe or tframed object.}

\usage{
    tfExpand(x, add.start = 0, add.end = 0)
    \method{tfExpand}{default}(x, add.start = 0, add.end = 0)
    \method{tfExpand}{tframe}(x, add.start = 0, add.end = 0)

    tfTruncate(x, start=NULL, end=NULL)
    \method{tfTruncate}{default}(x, start=NULL, end=NULL)
    \method{tfTruncate}{tframe}(x, start=NULL, end=NULL)
}
\arguments{
    \item{x}{A tframe or tframed object.}
    \item{start}{an integer indicating the position at which the new tframe is
        to start.}
    \item{end}{an integer indicating the position at which the new tframe is
        to end.}
    \item{add.start}{an integer indicating the number of periods on the
        beginning.}
    \item{add.end}{an integer indicating the number of periods on the end.}
 }
\value{A tframe or tframed object.}
\details{
    These methods are like tfwindow but use position indicators (rather 
    than dates) and work with a tframe or tframed data. Applied to a tframe
    they return an adjusted tframe. Applied to a tframed object
    they return an adjusted object with its adjusted tframe.They are low level
    utilities for other functions.
}
\seealso{
    \code{\link{tfwindow}}
    \code{\link{tframed}}
}

\examples{
    z <- ts(rnorm(100), start=c(1982,1), frequency=12)
    Dz <- tframed(diff(z), tfTruncate(tframe(z), start=2))
    tframe(Dz)
    IDz <- tframed(cumsum(c(0, Dz)), tfExpand(tframe(Dz), add.start=1))
    tframe(IDz)
    tframe(tfTruncate(z, start=5))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


