\name{trim.na}
\alias{trim.na}
\alias{trim.na.default}

\title{Trim NAs from Time Series}
\description{Trim NAs from the ends of a time series object.}
\usage{
	trim.na(obj, start.=T, end.=T)}
\arguments{
\item{obj}
{A time series matrix or an object of class TSdata.}
\item{ start.}{If start.=F then beginning NAs are not trimmed.}
\item{ end.} {If end.=F   then ending NAs are not trimmed.}
}
\value{A time series object which is windowed to the subset of 
 data which is not NAs (usually the available data).}
\details{
Trim NAs from the ends of a time series object.
Observations in a given period for all series are dropped if any one contains an NA.
}
\examples{
trim.na(ts(rbind(NA, matrix(1:20,10,2)), start=c(1980,1), frequency=12))
}
\keyword{tframe}
\keyword{time series data}

