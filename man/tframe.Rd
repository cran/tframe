\name{tframe}
\alias{as.tframe}
\alias{as.tframed}
\alias{tframe}
\alias{tframe.default}
\alias{tframe.ts}

\alias{tframe<-}
\alias{tfSet}
\alias{tfSet.default}
\alias{tfSet.list}
\alias{tfSet.tstframe}

\alias{tframed}
\alias{tframed.default}
\alias{is.tframe}
\alias{is.tframed}

\title{Extract or Set a tframe}
\description{Extract or set the tframe of an object.}

\usage{
    as.tframe(...)
    as.tframed(x)

    tframe(x)
    \method{tframe}{default}(x)
    \method{tframe}{ts}(x)

    tframe(x) <- value
    tfSet(value, x) 
    \method{tfSet}{default}(value, x) 
    \method{tfSet}{list}(value, x) 
    \method{tfSet}{tstframe}(value, x) 

    tframed(x, tf=NULL, names = NULL, ...)
    \method{tframed}{default}(x, tf = NULL, names = NULL, start=NULL, end=NULL, ...)

    is.tframe(x)
    is.tframed(x)
}

\arguments{
    \item{x}{an object (to which a tframe is assigned in assignment methods).}
    \item{value}{a tframe.}
    \item{tf}{a tframe object or a tframed object from which a tframe is taken.}
    \item{start}{provides simple way to specify a tframed time series similar
        to a \code{ts} object.}
    \item{end}{provides simple way to specify a tframed time series similar
        to a \code{ts} object.}
    \item{names}{optional vector of strings to specify new series names.}
    \item{...}{arguments passed to default to construct a tframe (rather than 
             extract one from x.)  \code{frequency} might often be used if 
	     \code{start} or \code{end} are specified.}
}
\value{Depends.}
\details{
    The usage \code{tframe(x)} returns the tframe of a tframed object. 
    The assignment
    \code{tframe(x)<-}, \code{tfSet}, and \code{tframed} set the tframe of an 
    object.
    \code{as.tframe(\dots)} constucts a tframe from \dots.
    \code{is.tframe} and \code{is.tframed} return 
    logicals if the argument is a tframe or tframed object respectively. 
    \code{as.tframed} guarantees \code{x} has a tframe by assigning a 
    default if \code{x} does not already have a tframe.
    
    The object of these functions is to be able to write code with
    \code{tframe(y) <- tframe(x)}, to assign the tframe attributes of
    \code{x} to \code{y}, without needing to handle details of the time 
    representation and without concern for the number of series in \code{x} 
    and \code{y}, which need not be the same. 
    A check is made to ensure the number of periods in the
    data correspond with the number implied by the tframe.
    
    There is an attempt to use the same time representation for \code{y} 
    as \code{x} has (e.g. ts, zoo, its), but this cannot be guaranteed 
    because \code{y} may not be 
    representable using the \code{x} represnetation. For example, \code{x} 
    might be an "mts" constructed with \code{ts()} whereas \code{y} is a 
    list with some data structures. In this case, a "pure tframe" approach 
    is used.
    
    The pure tframe approach sets a "tframe" attribute in object. This
    attribute has a class which indicates the time framing which is used. The 
    the time frame information is often secondary, in the sense that it does not
    describe the object structure, but only provides some additional information
    which is useful for doing time based operations on the data, plotting, and
    printing the object. By putting this in 
    an attribute, the objects class can be used for indicating other
    information about the structure of the object.  For
    these pure tframe objects the default \code{tframe} and \code{tframe<-} will
    often be adequate. The generic/method approach allows for special case (like
    TSdata where the tframe information is not an attribute of the object, but
    rather an attribute of the data matrices which are elements of the object).

    The generic/method approach also allows for (faking) tframe assignment and 
    extraction with classes like zoo, its, ts, and others which may appear,
    that try to make the time description part of the object class. (Not 
    a "tframe" approach.) The problem is to extract real tframes and
    also fake these other classes and old style tsp objects so they look 
    like tfamed objects. Another approach would be to mutilate these objects 
    and force them really be tframed objects (to have a tframe attribute), 
    but that risks conflicting with other (non tframe) code which used 
    the objects. This faking is accomplished by specific methods of the classes.
 
    The \code{tframed} constructor is simply a shortcut for assigning the tframe
    (tframe(x) <- tf) and series names (seriesNames(x) <-  names) to an object,
    but never assigns NULL values, so the result is guaranteed to be a 
    \code{tframed} object. It is like \code{ts} but enables the tframe library's  
    methods for handling time. If the \code{tf} argument is a 
    \code{tframed} object rather than a \code{tframe}, then the \code{tframe} 
    is extracted and used. If the \code{names} argument is not mode "character"
    of appropriate length, then \code{seriesNames(names)} is used. These make it
    simple to assign the time frame and names of one object to another by
    \code{z <- tframed(x, tf=y, names=y)}.
    
    \code{is.tframed} returns TRUE if a
    \code{tframe()} can extract a tframe from the object. This is true for many
    objects which are not truly tframed (like ts objects), since \code{tframe()}
    tries fairly hard to build a tframe for the object. 
}
\seealso{
    \code{\link{tfstart}},
    \code{\link{tfend}},
    \code{\link{tffrequency}},
    \code{\link{Tobs}},
    \code{\link{tftime}},
    \code{\link{tfL}}
}
\examples{
    z <- tframe(ts(rnorm(100), start=c(1982,1), frequency=12))
    z
    is.tframe(z)
    zz <- tframed(matrix(rnorm(200), 100,2), tf=z)
    is.tframed(zz)
    zzz <- tframed(matrix(rnorm(200), 100,2), tf=zz)
    is.tframed(zzz)
    tframe(zzz)
    
    as.tframe(start=c(1992,1), end=c(1996,3), frequency=4)
    Tobs(as.tframe(start=c(1992,1), end=c(1996,3), frequency=4))
    end(as.tframe(start=c(1992,1), end=c(1996,3), frequency=4))

    z <- tframed(rnorm(100), start=c(1982,1), frequency=12)
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}

