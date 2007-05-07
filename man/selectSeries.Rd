\name{selectSeries}
\alias{selectSeries}
\alias{selectSeries.default}
\alias{selectSeries.ts}

\title{Extract a Subset of Series}
\description{Extract a subset of series from a tframed object.}
\usage{
    selectSeries(x, series = seqN(nseries(x)))
    \method{selectSeries}{default}(x, series = seqN(nseries(x)))
    \method{selectSeries}{ts}(x, series = seqN(nseries(x)))
    }

\arguments{
    \item{x}{A tframed object.}
    \item{series}{The subset of series to retain.}
}
\value{A tframed object.}
\details{
    This is like [ , , drop=FALSE] but retains class, series name and tframe 
    information. It
    also provides a methods which works with multivariate series which 
    are not matrices (e.g. tfPADIdata).
}
\seealso{
    \code{\link{seriesNames}}
}
\examples{
    z <- selectSeries(matrix(rnorm(1000), 100,10), series=c(2, 5, 6))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


