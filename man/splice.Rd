\name{splice}
\alias{splice}
\alias{splice.default}
\title{Splice Time Series}
\description{
Splice together (in time dimension) two time series objects. This function can also be 
used to overlay obj1 on obj2 (obj1 takes precedence). The time windows do
not have to correspond.
}
\usage{
	splice(m1,m2)}
\arguments{
\item{obj1}{A time series object.}
\item{obj2}{A time series object.}
}
\value{A time series object}
\details{
Splice together two time series objects. The objects should contain the 
same number of time series variables and be arranged in the same order.
(e.g. - the first column of matrix obj1 is spliced to the first column of 
matrix obj2, etc.).
If data  is provided in both obj1 and obj2 for a given period then obj1 takes 
priority. The frequencies should be the same.
}

\seealso{
	\code{\link{tfwindow}},
	\code{\link{trim.na}},
	\code{\link{tbind}}
}

\examples{
splice(	ts(matrix(rnorm(24),24,1), start=c(1980,1), frequency=4),
	ts(matrix(rnorm(6),  6,1), start=c(1986,1), frequency=4))
}
\keyword{tframe}
\keyword{time series data}

