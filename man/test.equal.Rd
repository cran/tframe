\name{test.equal}
\alias{test.equal}
\alias{test.equal.array}
\alias{test.equal.default}
\alias{test.equal.list}
\alias{test.equal.matrix}
\alias{test.equal.numeric}
\alias{test.equal.tframe}
\alias{test.equal.tframe.default}
\alias{test.equal.tframe.stamped}

\title{Compare Two Objects}
\description{
Generic function to compare two objects.  The methods return a 
logical value, T if the objects are the same type and value and F otherwise.
The default compares array values but not attributes or class.
Some descriptive information in the objects may be ignored.
}
\usage{
	test.equal(obj1,obj2, fuzz=0)}
\arguments{
\item{ obj1, obj2}{Objects of the same class.}
\item{ fuzz}{Differences less than fuzz are ignored.}
}
\value{
T or F.
}
\details{
The functions for comparing numeric values used in the default method for this 
generic replacement.
}
\examples{
test.equal(matrix(1:10,10,2), array(1:10, c(10,2)))
test.equal(matrix(1:10,10,1),1:10)
}
\keyword{tframe}
\keyword{time series data}

