\name{earliestEnd}
\alias{earliestEnd}
\alias{earliestEndIndex}
\alias{earliestEndIndex.default}
\alias{earliestEndIndex.tframe}

\alias{earliestStart}
\alias{earliestStartIndex}
\alias{earliestStartIndex.default}
\alias{earliestStartIndex.tframe}

\alias{latestEnd}
\alias{latestEndIndex}
\alias{latestEndIndex.default}
\alias{latestEndIndex.tframe}

\alias{latestStart}
\alias{latestStartIndex}
\alias{latestStartIndex.default}
\alias{latestStartIndex.tframe}

\title{Start and End for Objects with Multiple Time Series}
\description{Return start or end date (or index of the object) from multiple
    time series objects.
}

\usage{
    earliestEnd(x, ...)
    earliestEndIndex(x, ...)
    \method{earliestEndIndex}{default}(x, ...)
    \method{earliestEndIndex}{tframe}(x, ...)

    earliestStart(x, ...)
    earliestStartIndex(x, ...)
    \method{earliestStartIndex}{default}(x, ...)
    \method{earliestStartIndex}{tframe}(x, ...)

    latestEnd(x, ...)
    latestEndIndex(x, ...)
    \method{latestEndIndex}{default}(x, ...)
    \method{latestEndIndex}{tframe}(x, ...)

    latestStart(x, ...)
    latestStartIndex(x, ...)
    \method{latestStartIndex}{default}(x, ...)
    \method{latestStartIndex}{tframe}(x, ...)

}

\arguments{
    \item{x}{A tframe or tframed object.}
    \item{...}{Additional tframe or tframed objects.}
}
\value{A date or index.}
\details{
    These functions calculate the start
    and end of each object in the argument and return a result by 
    comparing across objects. Thus, latestStart returns the start date of
    the object which starts latest and latestStartIndex returns the 
    corresponding index of the object in the argument list. 
}
\seealso{
    \code{\link{tframe}}
    \code{\link{tfwindow}}
    \code{\link{tfTruncate}}
    \code{\link{trimNA}}
}

\examples{
    t1<-ts(c(1,2,3,4,5), start=c(1991,1))
    t2<-ts(c(2,3,4,5,6,7,8), start=c(1992,1))
    t3<-ts(c(NA,2,3,4,5), start=c(1991,1))

    latestStart(t1,t2,t3)  # 1992 1 corresponding to the starting date of 
                            # the object which starts latest (t2)
    latestStart(t1,t3)     # both start in 1991 1 (NAs count as data)
    latestStart(tbind(t1,t2,t3)) # tbind gives a single object starting in 1991 1
    latestStart(t2, tbind(t1,t2,t3))

    latestStartIndex(t1,t2,t3)  # position of t2 in the argument list
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


