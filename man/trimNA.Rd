\name{trimNA}
\alias{trimNA}
\alias{trimNA.default}

\title{Trim NAs from Time Series}
\description{Trim NAs from the start and end of a time series object.}
\usage{
    trimNA(x, startNAs=TRUE, endNAs=TRUE)
    \method{trimNA}{default}(x, startNAs=TRUE, endNAs=TRUE)
    }
\arguments{
    \item{x}{A time series matrix or an object of class TSdata.}
    \item{startNAs}{If FALSE then beginning NAs are not trimmed.}
    \item{endNAs}{If FALSE   then ending NAs are not trimmed.}
}
\value{A time series object which is windowed to the subset of 
 data which is not NAs (usually the available data).}
\details{
Trim NAs from the ends of a time series object.
Observations in a given period for all series are dropped if any one contains an NA.
}
\examples{
trimNA(ts(rbind(NA, matrix(1:20,10,2)), start=c(1980,1), frequency=12))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


