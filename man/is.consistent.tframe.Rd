\name{is.consistent.tframe}
\alias{is.consistent.tframe}
\alias{is.consistent.tframe.default}
\alias{is.consistent.tframe.stamped}

\title{Check for a Consistent tframe}
\description{
	Check if tframe and a time series are consistent with one another.}
\usage{
	is.consistent.tframe(tf, x)}

\arguments {
	\item{x}{An object)}
}
\value{A logical scalar.}
\details{Check if the number of periods in the tframe corresponds to the
	number of observations in the time series.
}
\seealso{
	\code{\link{is.tframe}}
}

\examples{
	z <- ts(rnorm(100), start=c(1982,1), frequency=12)
	is.consistent.tframe(tframe(z), rnorm(100))
}
\keyword{tframe}
\keyword{ts}
