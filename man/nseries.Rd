\name{nseries}
\alias{nseries}
\alias{nseries.default}

\title{Number of Series}
\description{Return the number of series.}
\usage{
	nseries(x)}

\arguments {
	\item{x}{A time series object.}
}
\value{An integer.}
\details{
	Generic method to return the number of series.
}

\examples{
	nseries(tsmatrix(rnorm(100,20,5)))
}
\keyword{tframe}
\keyword{ts}

