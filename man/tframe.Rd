\name{tframe}
\alias{tframe}
\alias{tframe.default}
\alias{tframe.tf}
\alias{tframe.tframe<-.rts}
\alias{tframe.ts}
\alias{tframe.tstframe}
\alias{tframe<-}
\alias{tframe<-.TSdata}
\alias{tframe<-.default}
\alias{tframe<-.rts}
\alias{tframe<-.tf}
\alias{tframe<-.ts}
\alias{is.tframe}

\title{Extract or Set a Tframe}
\description{
	The first usage extracts the tframe from a tframed object. 
	These are generic functions. The default method attempts to build a
	tframe for time series, matrix, or array which is not tframed.
	The second usage assigns the tframe to an object.
}

\usage{
	tframe(x)
	tframe(x) <- tf
	is.tframe(x)
}

\arguments{
	\item{x}{A tframed object or an object to be tframed.}
	\item{tf}{A tframe.}
}
\value{A tframe.}
\details{
	The first usage returns the tframe of a tframed object. The second
	usage sets the tframe of an object.
}
\seealso{
	\code{\link{tframed}}
}
\examples{
	z <- tframe(ts(rnorm(100), start=c(1982,1), frequency=12))
	is.tframe(z)
}
\keyword{tframe}
\keyword{ts}
