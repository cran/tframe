\name{checktframeConsistent}
\alias{checktframeConsistent}
\alias{checktframeConsistent.default}

\title{Check for a Consistent tframe}
\description{
    Check if tframe and a time series are consistent with one another.}
\usage{
    checktframeConsistent(tf, x)
    \method{checktframeConsistent}{default}(tf, x)
    }

\arguments{
    \item{tf}{A tframe)}
    \item{x}{An object)}
}
\value{A logical scalar.}
\details{Check if the number of periods in the tframe corresponds to the
    number of observations in the time series.
}
\seealso{
    \code{\link{is.tframe}}
    \code{\link{periods}}
}

\examples{
    z <- ts(rnorm(100), start=c(1982,1), frequency=12)
    checktframeConsistent(tframe(z), rnorm(100))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


