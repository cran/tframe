\name{availability}
\alias{availability}
\alias{availability.default}
\alias{availability.tfPADIdata}
\title{Check Data Availability}
\description{Check the dates for which date is available.}
\usage{
	availability(data.id, verbose=T)
}
\arguments{
\item{data.id }
{ A vector of series identifiers or an object of 
class ("TSPADIdata", "TSdata"). See TSdata.TSPADIdata.}

\item{  verbose}{ T or F indicating if the results should be printed.}
\item{  names}
{ A character vector of names to be associated with the ids in printed results.}
\item{  server}
{ A character vector of servers to be associated with the ids.}
\item{  dbname}
{ A character vector of databases to be associated with the ids.}
}
\value{A list with elements start, end, and frequency.}
\details{
If verbose is T then the start, end, and frequency are printed for each 
series in data.id. A list is return (invisibly) with the same information. 
The default method works for a character vector argument. An argument of 
class TSPADIdata may supply some additional information, and several of 
the indicated optional arguments do not apply to that method.
}
\seealso{
\code{\link[dse2]{TSPADIdata}}
\code{\link[dse1]{TSdata}},
\code{\link[dse2]{set.TSPADIdata}}
\code{\link[dse2]{retrieve.and.verify.data}}
}
\examples{
if(require("padi")) {
   d <- tfPADIdata("D1", server="ets")
   availability(d)}
}
\keyword{tframe}
\keyword{time series data}

