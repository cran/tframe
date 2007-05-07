\name{tbind}
\alias{tbind}
\alias{tbind.default}
\alias{tbind.ts}

\title{Bind Time Series}
\description{
Bind together (in non-time dimension) two time series objects. 
}
\usage{
    tbind(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
    \method{tbind}{default}(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
    \method{tbind}{ts}(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
    }
\arguments{
    \item{x}{A time series object.}
    \item{...}{Time series objects.}
    \item{pad.start}{Logical indicating if the start should be truncated or
        padded with NAs to align time.}
    \item{pad.end}{Logical indicating if the end should be truncated or
        padded with NAs to align time.}
    \item{warn}{Logical indicating if warnings should be issued.}
}
\value{A time series object}
\details{
Bind data as in cbind (or formerly tsmatrix) and align time dimension. 
The default action pads series with NA to time union. 
If pad.start and/or pad.end is FALSE and the intersection is empty then NULL
is returned and a warning is issued if warn=TRUE.
}

\seealso{
    \code{\link{tfwindow}},
    \code{\link{trimNA}},
    \code{\link{splice}}
}

\examples{
tbind(    ts(matrix(rnorm(24),24,1), start=c(1986,1), frequency=4),
    ts(matrix(rnorm(6),  6,1), start=c(1986,1), frequency=4))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


