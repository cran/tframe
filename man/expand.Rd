\name{expand}
\alias{expand}
\alias{expand.default}
\alias{expand.tframe}
\alias{expand.tframe.default}
\alias{expand.tframe.tstframe}
\alias{truncate}
\alias{truncate.default}
\alias{truncate.tframe}
\alias{truncate.tframe.default}
\alias{truncate.tframe.tstframe}

\title{Expand a Tframe or Tframed Object.}
\description{Expand a tframe or tframed object.}

\usage{
	expand(x)
	truncate(x, start=NULL, end=NULL)
}
\arguments{
	\item{x}{A tframe or tframed object.}
	\item{start}{
		An integer indicating the position at which the new tframe is
		to start.
	}
	\item{end}{
		An integer indicating the position at which the new tframe is
		to end.
	}
}
\value{A tframe or tframed object.}
\details{
	These functions are like tfwindow but use position indicators (rather 
	than dates) and work with the tframe or tframed data. They are low level
	utilities for other functions.
}
\seealso{
	\code{\link{tfwindow}}
	\code{\link{tframed}}
}

\examples{
	tframe(truncate(ts(rnorm(100), start=c(1982,1), frequency=12), start=5))
}
\keyword{tframe}
\keyword{ts}

