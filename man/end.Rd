\name{end}
\alias{end.tf}
\alias{end.tfPADIdata}
\alias{end.tframe}
\alias{end.tframe.default}
\alias{end.tframe.tftframe}
\alias{end.tframe.tstframe}
\alias{frequency.tf}
\alias{frequency.tfPADIdata}
\alias{frequency.tframe}
\alias{frequency.tframe.default}
\alias{frequency.tframe.tftframe}
\alias{frequency.tframe.tstframe}
\alias{periods}
\alias{periods.default}
\alias{periods.tf}
\alias{periods.tfPADIdata}
\alias{periods.tframe}
\alias{periods.tframe.default}
\alias{periods.tframe.stamped}
\alias{periods.tframe.tftframe}
\alias{periods.tframe.tstframe}
\alias{periods.tsp}
\alias{start.tf}
\alias{start.tfPADIdata}
\alias{start.tframe}
\alias{start.tframe.default}
\alias{start.tframe.tftframe}
\alias{start.tframe.tstframe}

\title{Generic Time Series Parameters}
\description{
	Functions for extracting time information from tframes or 
	tframed objects.
}

\usage{
	start(x)
	end(x)
	periods(x)
	frequency(x)
}

\arguments{
	\item{x}{
		A tframe or a tframed object.
	}

}
\value{Depends}
\details{
	The methods start and end return the start or end date of a tframe or
	tframed object. Periods return the number of observations (time points).
	frequency returns the frequency of observation, 
	typically the number of observations in a year for economic data, but
	possibly something else in other contexts.  The concept of frequency is
	not very consistently defined for time series data, and the use of 
	the frequency method should probably be avoided where possible. In
	practice it seems rarely necessary, but the method makes porting of 
	older code much easier. 
}

\seealso{
	\code{\link{tframe}}, 
	\code{\link{tframed}}
}
\examples{
	z <- ts(rnorm(100), start=c(1982,1), frequency=12)
	start(z)
	end(z)
	periods(z)
}
\keyword{tframe}
\keyword{ts}

