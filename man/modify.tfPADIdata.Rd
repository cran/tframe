\name{modify.tfPADIdata}
\alias{modify.tfPADIdata}
\title{modify a database descriptor object}
\description{Modify an object with new information}
\usage{
	modify.tfPADIdata(obj, append=NA,
           series=NA, server=NA, db=NA, transforms=NA, names=NA,
           start=NA, end=NA, frequency=NA, 
           pad=NA, pad.start=NA, pad.end=NA,
           use.tframe=NA, stop.on.error=NA, warn=NA)
}
\arguments{
\item{ obj}{A tfPADIdata object.}
\item{ append}
{ If specified, append should be a list of (series, server, db,
transforms, names) as specified for tfPADIdata. These are appended as additional
series to the object.}
\item{ Other arguments as for tfPADIdata}
{See tfPADIdata descriptions. Values replace 
existing values (except in the case of append).}
}
\value{
A object of class "tfPADIdata" which can be used to retrieve a matrix time series object.
}
\details{Any specified (non NA) optional arguments are used to modify the object.
}
\seealso{
\code{\link{tfPADIdata}} 
\code{\link{freeze}}
}
\examples{
  z <- tfPADIdata( c("seriesA", "seriesB"), server="myserver")}
  z <- modify(z, pad.end=T )
 }
\keyword{tframe}
\keyword{time series data}

