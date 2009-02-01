\name{00tframe.Intro}
\alias{00.tframe.Intro}
\alias{tframe.Intro}
\alias{tframe-package}

\docType{package}

\title{Tframe - Generic Approach to Handling Time}

\description{Programs for implementing an object oriented approach to
    handling different time representations.
}

\details{
The \pkg{tframe} package provides a kernel of functions for programming time series 
methods in a way that makes them relatively independently of the representation 
of time. \pkg{tframe} is intended to make it easier to write code which can 
use any new/better 
time representations when they appear. It also provides plotting, time 
windowing, and some other utility functions which are specifically intended 
for time series. The main functions in this package that might be called 
directly by users are
\code{\link{tfplot}}, \code{\link{diffLog}}, \code{\link{percentChange}}, 
\code{\link{trimNA}} and
\code{\link{tsScan}}. See the help for more details and examples.

\pkg{tframe} provides generic methods
by which code can be developed without too much dependence on the
representation of time (i.e. specific time series objects). This can make
most code very robust with respect to other (and future)
improved/different representations of time. However, details
like putting the time axis label on a plot may require a method for the
the specific time representation. 

This package does not try to replace classes and methods associated with 
time representations such as \code{ts}, \code{zoo}, and \code{its}. Rather,
it attempts to provide generic programming "wrappers" so that other programs
do not need to look after the details of these different representations.
For many time series programs the availability of a \emph{window} method 
provided by those classes is the main method which is needed. However, 
the time attributes of calculated objects are
often lost and programmers must re-assign time attributes to the resulting
object if they are to be retained. Historically this was done with 
\code{tsp}, but this relies on a particular time representation and
would not work for other representations. In order to address this,
the \pkg{tframe} methods attempt to separate the time representation from 
the data and allow a statement 

\code{tframe(x) <- tframe(y)}

to make the time frame of \code{x} the same as that of \code{y}, 
without the need to
worry about what time representation is used in \code{y}.
In this assignment \code{x} and \code{y} need not be too similar (one
might be a univariate series while the other is a matrix or an array or
list of spatial or panel time series data), as long as they are similar
in the time dimension. For the case where \code{tsp(x) <- tsp(y)} would 
make sense,
that is effectively what the above tframe assignment will do. For some
existing code, most of the conversion to these more robust methods is
accomplished simply by changing "tsp" to "tframe" and nrow() for a time 
series matrix to periods().

The tframe assignment example above is accomplished by switching the 
dispatch so that it follows the classes of the tframe of \code{y}, rather than 
the classes of \code{x}, as would normally be done for the above kind of assignment.
Doing this in a generic way allows for the possibility
of future classes of time representation. This is different from the
way that zoo, its and ts are implemented, in the sense that it is the
tframe of the data which is assigned a class indicating the
time representation, not the data object itself.

The most general (last) class of the tframe should be \code{tframe}. 
The method \code{is.tframe}  checks if an object is a tframe, and 
the method \code{is.tframed} checks if an object has a tframe. 
In general, tframe methods act on the time frame (tframe) and
tframed methods act on data which is tframed.

More specific methods can be defined for any special time
representation. Methods are defined in this package for ts, zoo, and its,
and defaults work for old style tsp, and may also work in sme other cases. 
The tframe's specific classes are called tstframe, zootframe, and itstframe, 
to prevent confusion using inherit().

The main programing utilities are \code{tframe} and \code{tframe<-}.
For additinal details see the help for these and \code{tframe-package}.

The method tfwindow is used in this library and is typically just the same as
window, but the new name has been used because of historical changes and bugs
in window, and in order to support the argument "warn" to suppress  
messages (when objects are windowed unnecessarily).

One implication of a statement like
tframe(x) <- tframe(y)
is that the tframe should not indicate which dim of the data is the
time dimension. In general this will have to be another attribute of
the data. For older representations, the  convention of using the first 
dimension for
matrix data and the length for vector data, makes it unnecessary to
specify.

The attribute "seriesNames" is also supported as a way to indicate the names
of series in an object. This overlaps with the use of "names" and 
"dimnames[[2]]" used previously for series names in S, but seems necessary in
order to have a more complete generic decompostion of the time dimension from
the other dimensions. 

Many of the functions in the library are not yet individually documented, 
however, the functions
are all very short and can be examined easily. 
The code in the tests subdirectory provides a short set of tests and may
serve as an example.

To implementing a new time representation, suppose it is called zzz, 
then the tframe
attibute of an object using this time frame should have class c("zzz", "tframe").
(Note zzz should be different from the class of the object itself.)
The most important methods which need to be
implemented are tframe.zzz(), start.zzz(), end.zzz(), and periods.zzz().
While frequency.zzz() should not in theory be necessary, it makes 
porting code much easier. Other methods which may be needed are
time.zzz(), checktframeConsistent.zzz(), tfTruncate.zzz(), 
tfExpand.zzz(), 
earliestStartIndex.zzz(),
earliestEndIndex.zzz(), latestStartIndex.zzz(), and
latestEndIndex.zzz().

\tabular{ll}{
Package: \tab tframe\cr
Depends: \tab R\cr
License: \tab free, see LICENSE file for details.\cr
URL: \tab http://www.bank-banque-canada.ca/pgilbert\cr
}
}
\author{
Paul Gilbert <pgilbert@bank-banque-canada.ca>

Maintainer: Paul Gilbert <pgilbert@bank-banque-canada.ca>  
}

\seealso{
\code{\link{tframe}},
\code{\link{tframed}},
\code{\link{tfwindow}},
\code{\link{tfplot}}
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}


