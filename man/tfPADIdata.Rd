\name{tfPADIdata}
\alias{tfPADIdata}
\alias{is.tfPADIdata}
\alias{[.tfPADIdata}

\title{make tfPADIdata object}
\description{Make a tfPADIdata object from a vector of identifiers}
\usage{
	tfPADIdata( series, server="ets", db="", transforms="", names=NULL,
           start=NA, end=NA, frequency=NA, 
           pad=FALSE, pad.start=pad, pad.end=pad,
           use.tframe=T, stop.on.error=T, warn=T)
	is.tfPADIdata(obj)
}
\arguments{
\item{series}{Character vectors of series identifiers.}
\item{server db transforms names}
{ Character vectors of additional information corresponding to series 
identifiers. Scalar values are expanded to a vector of appropriate length.}
\item{start}{ a two element vector of starting year and period which 
is used when requesting data.}
\item{end}{ a two element vector of end year and end period which 
is used when requesting data.}
\item{frequency}{ a scalar indicating frequency. This is not 
used in requesting data, but if supplied it is used as a check of 
returned data. A warning is issued if returned data does not have this 
frequency.}
\item{pad}{ T or F indicating if data should be padded with NAs on both ends
to the length of the longest available series. If padding is not done 
then series are truncated to the intersection of available data so there 
are no NAs in the result. pad.start and pad.end are the arguments which are 
actually used. pad is just a convenient way to specify both.}
\item{pad.start}{ T or F indicating if NAs should be padded to beginning of data.}
\item{pad.end}{   T or F indicating if NAs should be padded to end of data.}
\item{names}{Names to replace series identifiers.}
\item{obj}{Any object.}
}

\value{
A object of class "tfPADIdata" which can be used to retrieve a matrix 
time series object.}
\details{
This is the way to produce an object which can be used for database access. 
The transforms a vector of strings, one for each series, giving any transformations which should be applied when the data is retrieved (with eval(call()), e.g. "log",  or "" if no transform is to be applied).
}
\seealso{
\code{\link{freeze}}
}
\examples{
	z <- tfPADIdata( c("seriesA", "seriesB"), server="myserver")
	is.tfPADIdata(z)
}
\keyword{tframe}
\keyword{time series data}

