\name{tfL}

\alias{tfL}
\alias{tfL.default}
\alias{tfL.tframe}

\alias{diff.tframe}
\alias{diff.tframed}

\title{Time Series Shifting and Differencing}
\description{Lag, shift forward, or difference a tframe or tframed object.}
\usage{
    tfL(x, p=1)
    \method{tfL}{default}(x, p=1)
    \method{tfL}{tframe}(x, p=1)

    \method{diff}{tframe}(x,lag=1, differences=1, ...)
    \method{diff}{tframed}(x,lag=1, differences=1, ...)
}
\arguments{
    \item{x}{a tframed object.}
    \item{lag}{difference calculated relative to lag periods previous.}
    \item{differences}{order of differencing.}
    \item{p}{number of periods to shift or lag periods for differencing.}
    \item{...}{arguments to be passed to other methods.}
}
\details{
   \code{tfL} methods shift the time frame, or the time frame of the object, by
   \code{p} periods. (This might also be thought of as the exponent of the lag 
   operator.) Positive \code{p} means shift the time frame forward and negative
   \code{p} means shift the time frame back. Shifting the time frame forward
   means the data at a point in time is the data from the previous point in time
   in the unshifted data, so the result corresponds to what is often called 
   the lagged data. The default \code{p} (+1) means 
   the start and end for the results are one period later. When applied to a
   data object, the default result is one lag of the data.
   This convention is not the same as that used for \code{k} in the 
   function \code{lag}. 

   Note that the time frame of the data is shifted, but a vector or matrix
   representation of the data is unchanged. This means that operations on the
   data, such as \code{+, -, *, and /},  need to be time aware as, for example,
   operations on ts objects are. If the operations do not recognize the time
   framed aspect of the objects, then the operation will be performed with
   default methods that will probably have unintended results (see examples).
   
   Differencing methods create a time frame or time framed object 
   by differencing the number 
   of times indicated by \code{differences} at a lagged number of periods indicated 
   by \code{lag}. (Positive values of \code{lag} indicate number of periods back.)
   The default is take the difference from data one period previous.
   See \code{\link[base]{diff}} for more details, but note that the result when 
   applied to a time frame is a time frame, not a series.
}
\seealso{
    \code{\link[base]{diff}},
    \code{\link[stats]{lag}}
}
\examples{
    z <- ts(rnorm(100), start=c(1982,1), frequency=12)
    tfstart(z)
    Tobs(z)
    z <- diff(z)
    tfstart(z)
    Tobs(z)

    ts(1:5) - tfL(ts(1:5))
    (1:5) - tfL(1:5) # (1:5) this is not a tframed object, so minus is the default
    ts(1:5) - tfL(ts(1:5), p= 2)

    z <- ts(1:10, start=c(1992,1), frequency=4)
    z - tfL(z)

    z <- ts(matrix(1:10,5,2), start=c(1992,1), frequency=4)
    seriesNames(z) <- c("One", "Two")
    z - tfL(z)
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


