\name{identifiers}
\alias{identifiers}
\alias{identifiers.default}
\alias{identifiers.tfPADIdata}
\title{Get Identifiers from a data object}
\description{Get Identifiers from a data object}
\usage{
	identifiers(data.names)
}
\arguments{
\item{ data.names}
  {An object which contains source series identifier information.}
}
\value{
A list with elements input and output which are strings indicating the 
input and output series identifiers.
}
\seealso{ 
\code{\link{tfPADIdata}}
\code{\link[dse2]{TSPADIdata}}
\code{\link{source.info}}
\code{\link{sourcedb}}
}
\examples{
  d <- tfPADIdata("D1", server="ets")
  identifiers(d)
 }
\keyword{tframe}
\keyword{time series data}

