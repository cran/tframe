\name{nseries}
\alias{nseries}
\alias{nseries.default}

\title{Number of Series}
\description{Return the number of series.}
\usage{
    nseries(x)
    \method{nseries}{default}(x)
    }

\arguments{
    \item{x}{A time series object.}
}
\value{An integer.}
\details{
    Generic method to return the number of series.
}

\examples{
    nseries(tbind(rnorm(100,20,5)))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


