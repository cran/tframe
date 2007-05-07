\name{testEqualtframes}
\alias{testEqualtframes}
\alias{testEqualtframes.default}
\alias{testEqualtframes.stamped}

\title{Compare Two Time Frames}
\description{
Generic function to compare two time frames.  The methods return a 
logical value, TRUE if the time frames are the same type and value and FALSE otherwise.
}
\usage{
    testEqualtframes(tf1,tf2)
    \method{testEqualtframes}{default}(tf1,tf2)
    \method{testEqualtframes}{stamped}(tf1,tf2)
    }
\arguments{
    \item{tf1, tf2}{Time frames of the same class.}
}
\value{TRUE or FALSE}
\details{
Time frames are compared. Time frames need to be of the same class (although it would be nice if they did
not need to be).
}
\examples{
testEqualtframes(tframe(matrix(1:10,10,2)), tframe(array(1:10, c(10,2))))
}
\seealso{
    \code{\link{testEqual}}
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}


