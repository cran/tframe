\name{series.names}
\alias{series.names}
\alias{series.names.default}
\alias{series.names.tfPADIdata}
\alias{series.names<-}
\alias{series.names<-.default}

\title{Names of Series in a tframed object}
\description{Extract or set names of series in a tframed object.}
\usage{
	series.names(data)
	series.names(data) <- names 
}
\arguments{
\item{data}{A tframed object.}
}
\value{
The first usage gives a vector of strings with the series names.
The second usage assigns a vector of strings to be the series names of data.
}
\seealso{
	\code{\link{tframed}},
	\code{\link[dse1]{input.series.names}},
	\code{\link[dse1]{output.series.names}}
}

\examples{
	z <- matrix(rnorm(100), 50,2)
	series.names(z) <- c("a", "b")
	series.names(z)
}
\keyword{tframe}
\keyword{ts}

