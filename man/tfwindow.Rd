\name{tfwindow}
\alias{tfwindow}
\alias{tfwindow.default}
\alias{tfwindow.tf}

\title{Truncate a Time Series}
\description{Truncate a time series object to a time window.}
\usage{
	tfwindow(x, start.=NULL, end.=NULL, warn=T)}

\arguments{
\item{x}{A time series object.}
\item{start.}{A start date of a format compatible with the time series}
\item{end.}   {An end date of a format compatible with the time series}
\item{warn}{A logical indicating if warning should be produced}
}
\value{
A time series object similar to x, but typically spanning a shorter time period.
}
\details{
This function calls window but makes more effort to preserve \code{series.names}
if the \code{x} has them. It also supports the optional 
argument \code{warn} to suppress
warning messages. Frequently it is convenient to write code which always 
truncates to a window without first checking if the data is already within 
the window. Since window produces a warning in this situation, the optional 
argument is frequently useful when tfwindow is used by other code.
}
\examples{
tfwindow(ts(matrix(rnorm(24),24,1), start=c(1980,1), frequency=4), 
	start=c(1982,2))
}
\keyword{tframe}
\keyword{time series data}

