\name{time}
\alias{time}
\alias{time.tf}
\alias{time.tframe}
\alias{time.tframe.default}
\alias{time.tframe.tftframe}
\alias{time.tframe.tstframe}

\title{Time}
\description{The time coresponding to each data point.}
\usage{
	time(x)}

\arguments{
	\item{x}{A tframe or tframed object.}
}
\value{A time series of the dates of observations of x.}
\details{
	This returns the time (date) of each observation, as might be used on
	the axis of a graph or lables of a printout.
}
\seealso{
	\code{\link{periods}}
	\code{\link{tframe}}
}
\examples{
	z <- tframed(matrix(rnorm(200), 100,2), 
			tf=list(start=c(1982,1), frequency=12))
	time(z)
}
\keyword{tframe}
\keyword{ts}

