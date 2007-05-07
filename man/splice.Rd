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
    splice(mat1,mat2, ...)
    \method{splice}{default}(mat1,mat2, ...)
    }
\arguments{
    \item{mat1}{A time series object.}
    \item{mat2}{A time series object.}
    \item{...}{arguments to be passed to other methods (not used by the default
       method).}
}
\value{A time series object}
\details{
Splice together two time series objects. The mat1 and mat2 objects should contain the 
same number of time series variables and be arranged in the same order.
(e.g. - the first column of mat1 is spliced to the first column of 
mat2, etc.).
If data  is provided in both mat1 and mat2 for a given period then mat1 takes 
priority. The frequencies should be the same.
}

\seealso{
    \code{\link{tfwindow}},
    \code{\link{trimNA}},
    \code{\link{tbind}}
}

\examples{
splice(ts(matrix(rnorm(24),24,1), start=c(1980,1), frequency=4),
       ts(matrix(rnorm(6),  6,1), start=c(1986,1), frequency=4))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


