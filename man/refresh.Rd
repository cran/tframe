\name{refresh}
\alias{refresh}
\title{Retrieve new data from a database}
\description{
Extact source information and retreive an updated version of 
the data from a database.
}
\usage{
	refresh(data)
}
\arguments{
\item{data}
{An object which has previously been obtained with \code{freeze}, 
so it contains source information which can be extracted and freeze re-applied.}
}
\value{A time series object with class depending on the source information.}
\examples{
if(require("padi")) {
   d <- tfPADIdata("D1", server="ets")
   d <- freeze(d)
   d <- refresh(d) }
}
\keyword{tframe}
\keyword{time series data}
