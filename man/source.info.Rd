\name{source.info}
\alias{source.info}
\alias{source.info.default}
\alias{source.info.tfPADIdata}
\title{Get source information from a data object}
\description{Get source information from an object}
\usage{
	source.info(data.names)
}
\arguments{
\item{ data.names}
  {An object which contains source series information.}
}
\value{
A list with elements input and output which are strings indicating the 
input and output source.
}
\seealso{
\code{\link[dse2]{TSPADIdata}} 
\code{\link[dse2]{identifiers}}
}
\examples{
if(require("padi")) {
   d <- tfPADIdata("D1", server="ets")
   d <- freeze(d)
   source.info(d)}
}
\keyword{tframe}
\keyword{time series data}

