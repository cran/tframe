\name{earliest.end}
\alias{earliest.end}
\alias{earliest.end.index}
\alias{earliest.end.index.tframe}
\alias{earliest.end.index.tframe.default}
\alias{earliest.end.index.tframe.tstframe}
\alias{earliest.end.tframe}
\alias{earliest.start}
\alias{earliest.start.index}
\alias{earliest.start.index.tframe}
\alias{earliest.start.index.tframe.default}
\alias{earliest.start.index.tframe.tstframe}
\alias{earliest.start.tframe}
\alias{latest.end}
\alias{latest.end.index}
\alias{latest.end.index.tframe}
\alias{latest.end.index.tframe.default}
\alias{latest.end.index.tframe.tstframe}
\alias{latest.end.tframe}
\alias{latest.start}
\alias{latest.start.index}
\alias{latest.start.index.tframe}
\alias{latest.start.index.tframe.default}
\alias{latest.start.index.tframe.tstframe}
\alias{latest.start.tframe}

\title{Start and End for Ragged Multivariate Time Series}
\description{Return start or end date (or index of the point) for multivariate
	time series that do not have the same time window. NAs which pad some
	series to make a matrix are not counted.
}

\usage{
	earliest.end(x)
	earliest.end.index(x)
	earliest.start(x)
	earliest.start.index(x)
	latest.end(x)
	latest.end.index(x)
	latest.start(x)
	latest.start.index(x)
}

\arguments{
	\item{x}{A tframe or tframed object.}
}
\value{A date or index.}
\details{
	Individual series in multivariate time series do not always start 
	and end at the
	same period. For example, in order to store series in a matrix, 
	individual series are padded with NA. These functions discount 
	the NAs and calculate the start
	and end of data in each series, then return a result by 
	comparing across series. Thus, latest.start returns the start date of
	the series which starts latest and latest.start.index returns the 
	corresponding index (row position in the matrix). The index functions
	should be considered lower level access to the data structure and should
	not typically be used as much as the functions which return dates. 
}
\seealso{
	\code{\link{tframe}}
	\code{\link{tfwindow}}
	\code{\link{truncate}}
}

\examples{
	z <- tbind(ts(rnorm(100), start=c(1982,1), frequency=12),
		   ts(rnorm(100), start=c(1986,1), frequency=12))
	earliest.end(z)
	latest.start(z)
	latest.start.index(z)
}
\keyword{tframe}
\keyword{ts}
