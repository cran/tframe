\name{sourceserver}
\alias{sourceserver}
\alias{sourceserver.default}
\alias{sourceserver.tfPADIdata}

\title{Get source server from a data object}
\description{Get source server information from an object}
\usage{
	sourceserver(data.names)
}
\arguments{
\item{ data.names}{
  {An object which contains source series information.}
}
\value{
A list with elements input and output which are strings indicating the 
input and output sourceserver.
}
\seealso{
\code{\link[dse2]{TSPADIdata}} 
\code{\link{identifiers}}
\code{\link{sourcedb}}
\code{\link{source.info}}
}
\examples{
if(require("padi")) {
   d <- tfPADIdata("D1", server="ets")
   sourceserver(d)}
}
\keyword{tframe}
\keyword{time series data}

