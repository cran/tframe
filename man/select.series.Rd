\name{select.series}
\alias{select.series}
\alias{select.series.default}

\title{Extract a Subset of Series}
\description{Extract a subset of series from a tframed object.}
\usage{
	select.series(x, series=seq(ncol(x)))}

\arguments {
	\item{x}{A tframed object.}
	\item{series}{The subset of series to retain.}
}
\value{A tframed object.}
\details{
	This is like [ , , drop=F] but retains class, series name and tframe 
	information. It
	also provides a methods which works with multivariate series which 
	are not matrices (e.g. tfPADIdata).
}
\seealso{
	\code{\link{series.names}}
	\code{\link{tfPADIdata}}
}
\examples{
	z <- select.series(matrix(rnorm(1000), 100,10), series=c(2, 5, 6))
}
\keyword{tframe}
\keyword{ts}

