\name{00General_Description}
\alias{00General_Description}
\title{Tframe}

\center{Generic Methods for Handling Time}

\description{
The \emph{tframe} library facilitates an object oriented approach
to handling different time representations in S/R. It provides generic methods
by which code can be developed without too much dependence on the
representation of time (i.e. specific time series objects). This can make
most code very robust with respect to other (and future)
improved/different representations of time. However, something like putting
the time axis label on a plot will require a method which is specific
to the time representation. The methods also provide a
simple way to fix some inconsistency which have occured with the
mix of possible time representations.

The classes and methods associated with the implementation of rts, cts
and its in Splus and ts in R accomplish some of these objectives. 
For many time series programs the availability of a \emph{window} method 
provided by those classes is the main method which is needed. However, after
calculations are done the time attributes of the resulting objects are
often lost and the programmer must re-assign time attributes to the resulting
object if those are to be retained. This has typically been done with something
like tsp(), but these function relies on a particular time representation and
would not always make sense for another representation. In order to address this,
the tframe methods attempt to further separate the time representation from 
the data to allow statements like

tframe(x) <- tframe(y)

to make the time frame of x the same as that of y, without the need to
worry about what time representation is used in y (e.g. ts, rts, cts,
its, ...). In this assignment x and y need not be too similar (one
might be a univariate series while the other is a matrix or an array or
list of spatial or panel time series data), as long as they are similar
in the time dimension. For the case where tsp(x) <- tsp(y) would make sense,
that is effectively what the above tframe assignment will do, and for 
existing code, most of the conversion to these more robust methods is
accomplished simply by changing "tsp" to "tframe" and nrow() for a time 
series matrix to periods().

The tframe assignment example above is accomplished by switching the 
dispatch so that it follows the classes of the tframe of y, rather than 
the classes of x as would normally be done for the above kind of assignment.
Doing this in a generic way allows for the possibility
of future classes of time representation. This is different from the
way that rts, cts, its and ts are implemented, in the sense that it is the
tframe of the data which is assigned a class indicating the
time representation, not the data object itself.

The most general (last) class of the tframe should be "tframe". 
The method "is.tframe"  checks if an object is a tframe, and 
the method "is.tframed" checks if an object has a tframe. 
In general, tframe methods act on the time frame (tframe) and
tframed methods act on data which is tframed.

More specific methods can be defined for any special time
representation (methods are defined in this library for tframes of class
c("default", "tframe") which in Splus are the old style tsp convention for time).
Also, there is a sketched implementaion for rts, cts,
its, and a class and methods style implementation of tsp called tf. The
tframe's specific classes are called rtstframe, ctstframe, and itstframe, 
to prevent confusion using inherit().
The R version includes an implementaion for the ts time series class with
tframe's specific class is tstframe.

When implementing a new class, suppose it is called zzz, the most 
important methods which need to be
supported are tframe.zzz(), start.tframe.zzz(), end.tframe.zzz(), and 
periods.tframe.zzz().
While frequency.tframe.zzz() should not in theory be necessary, it makes 
porting code much easier. Other methods which should be supported are
time.tframe.zzz(), is.consistent.tframe.zzz(), truncate.tframe.zzz(), 
expand.tframe.zzz(), 
earliest.start.index.tframe.zzz(),
earliest.end.index.tframe.zzz(), latest.start.index.tframe.zzz(), and
latest.end.index.tframe.zzz().

The method tfwindow is used in this library and is typically just the same as
window, but the new name has been used because of historical changes and bugs
in window, and in order to support the argument "warn" to suppress  
messages (when objects are windowed unnecessarily).

One implication of the desire to be able to use a statement like
tframe(x) <- tframe(y)
is that the tframe should not indicate which dim of the data is the
time dimension. In general this will have to be another attribute of
the data, but the current convention of using the first dimension for
matrix data and the length for vector data, makes it unnecessary to
specify.

Operations which should be possible on tframe'd data:

  In the time dimension
  
     - window (tfwindow), splice 
     
  In other dimensions
  
     -tbind  (like cbind) with shift to align the the tframe 
                 (and NA pad.start, pad.end, pad=T/F) 
		 
     - []   without losing the tframe. This is done with select.series().


The attribute "series.names" is also supported as a way to indicate the names
of series in an object. This overlaps with the use of "names" and 
"dimnames[[2]]" used previously for series names in S, but seems necessary in
order to have a more complete generic decompostion of the time dimension from
the other dimensions. 

Many of the functions in the library are not yet individually documented 
(and were it exists the documentation is a draft) however, the functions
are all very short and can be examined easily. 
The function \code{tframe.function.tests} provides a short set of tests and may
serve as an example.
}
\examples{tframe.function.tests()}

\keyword{programming}
\keyword{interface}
\keyword{utilities}
\keyword{ts}
\keyword{models}
\keyword{neural}
\keyword{tframe}

\author{Paul Gilbert}

