\name{testEqual}
\alias{testEqual}
\alias{testEqual.default}
\alias{testEqual.array}
\alias{testEqual.list}
\alias{testEqual.matrix}
\alias{testEqual.numeric}

\title{Compare Two Objects}
\description{
Generic function to compare two objects.  The methods return a 
logical value, TRUE if the objects are the same type and value and FALSE otherwise.
The default compares array values but not attributes or class.
Some descriptive information in the objects may be ignored.
}
\usage{
    testEqual(obj1, obj2, fuzz = 0)
    \method{testEqual}{default}(obj1, obj2, fuzz = 1e-16)
    \method{testEqual}{array}(obj1, obj2, fuzz = 1e-16)
    \method{testEqual}{list}(obj1, obj2, fuzz = 1e-16)
    \method{testEqual}{matrix}(obj1, obj2, fuzz = 1e-16)
    \method{testEqual}{numeric}(obj1, obj2, fuzz = 1e-16)
    }
\arguments{
    \item{obj1, obj2}{Objects of the same class.}
    \item{fuzz}{Differences less than fuzz are ignored.}
}
\value{
TRUE or FALSE.
}
\details{
The functions for comparing numeric values used in the default method for this 
generic replacement.
}
\examples{
testEqual(matrix(1:10,10,2), array(1:10, c(10,2)))
testEqual(matrix(1:10,10,1),1:10)
}
\seealso{
    \code{\link{testEqualtframes}}
}
\keyword{programming}
\keyword{utilities}


