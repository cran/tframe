\name{add.date}
\alias{add.date}
\title{Add Periods to a Date}
\description{
	Add periods to two element start date of given frequency to give a
	new date. NULL periods is treated as 0.
}
\usage{
	add.date <- function(date, periods, freq)
}
\arguments {
	\item{date}{A two element date as used by tsp i.e c(year, period).}
	\item{periods}{A number of periods.}
	\item{freq}{The number of periods in a year.}
}
\value{A two element date.}

\seealso{
	\code{\link{expand}}
}
\examples{
	add.date(c(1998,1), 20, 12)
}

\note{A useful utility not strictly part of tframe.}

\keyword{tframe}
\keyword{ts}
