\name{set.tfPADIdata}
\alias{set.tfPADIdata}
\title{Prompt for series names}
\description
{Prompt for data series names and locations and create a data object.}
\usage{
	set.tfPADIdata()
}
\value{An object of class 'tfPADIdata' (or retrieved time series matrix).}
\details{
The function prompts for the names and database locations of series.
The data can be retrieved, in which case it is returned as currently available 
on the database (and becomes a time series matrix), or it may be left dynamic, in which case it is of class 'tfPADIdata' and the discription can be used anytime to retrieve data.
}
\seealso{
\code{\link{freeze}}
\code{\link[dse1]{TSdata}}
\code{\link[dse1]{TSdata.object}}
}
\examples{
  data <- set.tfPADIdata()
 }
\keyword{tframe}
\keyword{time series data}

