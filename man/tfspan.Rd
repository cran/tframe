\name{tfspan}

\alias{tfspan}

\title{Time Span}
\description{Calculate Time Span of Objects.}

\usage{
    tfspan(x, ...)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{...}{other tframes or tframed objects.}
}
\value{A tframe}
\details{
    Calculate the time frame from the earliest start to latest end of all
    arguments. 
}

\seealso{
    \code{\link{tframe}}, 
    \code{\link{tframed}}
    \code{\link{start}}
    \code{\link{end}}
    \code{\link{frequency}}
    \code{\link{Tobs}}
    \code{\link{time}}
}
\examples{
    z  <- ts(rnorm(100), start=c(1982,1), frequency=12)
    zz <- ts(rnorm(100), start=c(1992,1), frequency=12)
    tfspan(z, zz)
}

\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


