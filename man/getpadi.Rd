\name{getpadi}
\alias{getpadi}
\title{Get Data from TSPADI Database Interface}
\description{Get data from a TSPADI database interface.}
\usage{
	getpadi <-function(series,server=local.host.netname(), dbname="",
                start.server=T, server.process=padi.server.process(),
                cleanup.script=padi.cleanup.script(),
                starty=0,startm=0,startd=1, endy=0,endm=0,endd=1, 
                nobs=0,max.obs=2000, transformations=NULL, pad=F,
                user=user.name(), passwd="",
                stop.on.error=T, use.tframe=T, warn=T, timeout=60)

}
\arguments{
\item{series}
{A character string giving the name of the series. Alternately, series can be a
vector of character strings specifying multiple series.}
\item{server}
{A character string giving the network name of the server which is to be requested to supply the series.  If series specifies multiple series and they are not all on the
same server then server should be a vector of character strings with
elements corresponding to the elements of series.
\item{dbname}
{
A character string giving additional information to the server about the location of the series (eg. the name of a database).  If series specifies multiple series and they are not all on the
same database then dbname should be a vector of character strings with
elements corresponding to the elements of series.
\item{starty}{An integer indicating the starting year.}
\item{startm}{An integer indicating the starting period.}
\item{startd}{An integer indicating the starting day.}
\item{endy}{An integer indicating the ending year.}
\item{endm}{An integer indicating the ending period.}
\item{endd}{An integer indicating the ending day.}
\item{nobs}{The number of observations.}
\item{transformations}
{A character string giving transformations to be applied to the series 
(e.g. "log"). If multiple series are being requested then transformations 
can be a single string, in which case it is applied to all series, or a
vector of character strings, one for each series. If no transformation is to
be applied to some series then "" should be used.}
\item{pad}
{
If FALSE (default) then all series are truncated to the interection of 
available time periods (i.e. the latest start date and earliest end date).
If TRUE then series are padded with NA so the result
starts at the earliest available observation and ends at the last available 
observation.}
\item{start.server}{try to start a server if one is not running}
\item{server.process}{command to execute in an attempt to start a server}
\item{cleanup.script}{command to execute to terminate a server if one is started}
\item{user}{user id for access to the database (if necessary)}
\item{passwd}{password for access to the database (if necessary)}
\item{stop.on.error}
{if T then stop is executed when an error occurs. Otherwise, the
error message is returned and the calling program must deal with it.}
\item{use.tframe}
{If use.tframe=F then ts() is used to construct the time series, otherwise
the tframe utilities are used.
Certain transformations available with DSE require the tframe stucture
and an error may result if these transformations are attempted with
use.tframe=F.}
\item{warn }
{print warning messages for some crude frequency conversions (weekly data).}
}
\value{A time series matrix with a column for each series.}
\details{
This description is provided with DSE so the help will be
integrated. The function is available separately (with the PADI
interface at http://www.bank-banque-canada.ca/pgilbert) and may not be
implemented on your system. This description may also be out-of-date
with respect to the current version of the function. Print getpadi for
more accurate information.

start. and end dates or start dates and nobs can be supplied.  If all
are set to zero (the default) then all data is retrieved, provided
max.obs is large enough.  If more than one series is to be returned
then series should be a vector of strings. In this case dbname must be
a vector of corresponding length or a single string which is applied to
all series. All series must have the same frequency.  If the number of
observations in any single series is larger than max.obs then an error
will occur and max.obs should be set larger. For many data frequencies
startd and endd can be omitted.
}
\examples{
if(require("padi")){
  cpi <-getpadi("I37026", server= "ets")
  cpi <-getpadi("I37026", starty=1988, startm=1,endy=1990, endm=12, server= "ets")
  cpi <-getpadi( "I37026",starty=1988, startm=1,nobs=8, server= "ets")
  data <- getpadi( c("B14017","B1627","I37026"), server= "ets" )
 }
}
\keyword{tframe}
\keyword{time series data}

