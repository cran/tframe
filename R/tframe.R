
classed <- function(x, cls) {class(x) <- cls; x}
# structure would work to replace classed (but adds some overhead).
#classed <- function(x, cls) structure(x, class=cls)

###########################################################################

#internal utility
# Use this with "for (i in seq(length=m) )" as m==0 returns NULL and for does no loops
seqN <- function(N) {if (0==length(N)) NULL else if (N<=0) NULL else seq(N)}



# start, end, frequency, time need to be masked from R base so that
# tframe methods can work on the tframe attribute rather than class(x)

# The .tframe methods are "default" methods for tframes. Other more specific
#  methods can be defined (see eg start.tstframe for tframes from ts objects). 


# periods should give the number of data points in the time direction.
# for consistency check periods needs to look at the data not the tframe,
# i.e. the number of (vector) observations.
periods <- function(x) UseMethod("periods")
periods.default <- function(x) {if (is.array(x)) dim(x)[1] else length(x) }

# the functions start, end, and frequency in tframe and dse do not 
#  need "...", but the generic in R has it, so it is added here.

start.tframed     <- function(x, ...) tfstart(tframe(x)) 
end.tframed       <- function(x, ...) tfend(tframe(x)) 
frequency.tframed <- function(x, ...) tffrequency(tframe(x)) 
time.tframed      <- function(x, ...) tftime(tframe(x)) 
periods.tframed   <- function(x)      tfperiods(tframe(x)) 

start.tframe     <- function(x, ...) tfstart(x)
end.tframe       <- function(x, ...) tfend(x) 
frequency.tframe <- function(x, ...) tffrequency(x) 
time.tframe      <- function(x, ...) tftime(x) 
periods.tframe   <- function(x)      tfperiods(x)

tfstart     <- function(x) UseMethod("tfstart")
tfend       <- function(x) UseMethod("tfend")
tffrequency <- function(x) UseMethod("tffrequency")
tftime <- function(x) UseMethod("tftime")
tfperiods <- function(x) UseMethod("tfperiods")


# these server two purposes. One is a method for tframe's. Two is a consistent
#programming method with tfstart(NULL) returning NULL (which start does not).
tfstart.default     <- function(x) 
  {if (is.null(x)) return(NULL) else
   if (!is.tframe(x)) x <- tframe(x)
   c(floor(x[1]), round(1 +(x[1]%%1)*x[3]))
  }
tfend.default       <- function(x)
  {if (is.null(x)) return(NULL) else
   if (!is.tframe(x)) x <- tframe(x)
   c(floor(x[2]), round(1 + (x[2]%%1)*x[3]))
  }
tffrequency.default <- function(x)
  {if (is.null(x)) return(NULL) else
   if (!is.tframe(x)) x <- tframe(x)
   x[3]
  }
tftime.default      <- function(x)
  {if (is.null(x)) return(NULL) else
   if (!is.tframe(x)) x <- tframe(x)
   tframed(x[1]+(seq(periods(x))-1)/x[3], tf=x)
  }
tfperiods.default   <- function(x)
  {if (is.null(x)) return(NULL) else
   if (!is.tframe(x)) x <- tframe(x)
   1+round((x[2]-x[1])*x[3])
  }


tfL <- function(x, p=1) UseMethod("tfL") 
 
tfL.tframe <- function(x, p=1){ x + (p/x[3]) * c(1, 1, 0)}

tfL.default <- function(x, p=1){
    tframe(x) <- tfL(tframe(x), p=p)
    x} 


diff.tframed <- function(x, lag=1,   differences=1, ...)
   {tf <- diff(tframe(x), lag=lag, differences=differences) 
    tframe(x) <- NULL
    tframed(diff(x, lag=lag, differences=differences), tf)
    }

diff.tframe <- function (x,lag=1, differences=1, ...) 
 {d <- lag * differences
  tfTruncate(x, start=if(d >= 0) 1+d else NULL, 
                  end=if(d <  0) periods(x)-d else NULL)
 }

#  tfplot and tfprint below provide generic methods for plotting and printing
#  tf time series objects. Plot methods will probably do some processing
#  and eventually call tfplot.default.

tfplot <- function(x, ...)  UseMethod("tfplot")

tfspan <- function(x, ...)
  {others <- list(...)
   tfspan <- x
   #this is a kludge to get the overall time span from the result of tbind.
   if (0 != length(others)) for (d in others) tfspan <- tbind(tfspan , d)
   tframe(tfspan)
  }


tfplot.default <- function(x, ..., tf=tfspan(x , ...), start=tfstart(tf), end=tfend(tf),
       series=seq(nseries(x)), Title=NULL,
       lty = 1:5, lwd = 1, pch = NULL, col = 1:6, cex = NULL,
       xlab=NULL, ylab=seriesNames(x), xlim = NULL, ylim = NULL,
       graphs.per.page=5, par=NULL, mar=par()$mar, reset.screen=TRUE)
 {#  ... before other args means abbreviations do not work, but otherwise
  # positional matching seems to kick in and an object to be plotted gets used
  #  for start.
  if (!is.tframed(x)) UseMethod("plot")
  else
    {if(inherits(x, "TSmodel"))
        stop("tfplot does not know how to plot a model. ",
             "Consider simulating the model: tfplot(simulate(model)) ",
             "or evaluating the model with data: tfplot(l(model, data)).")
     if( !is.numeric(x) )
        stop("tfplot.default does not know how to plot this object.")
     old.par <- par(par)
     on.exit(par(old.par)) 
     names <- seriesNames(x)
     Ngraphs <- min(length(series), graphs.per.page)
     if( (!is.list(xlim)) && (2 == length(xlim)))
              xlim <- rep(list(xlim), length(series))
     if( (!is.list(ylim)) && (2 == length(ylim)))
              ylim <- rep(list(ylim), length(series))
     if(reset.screen)  {
        if ( (! is.null(par)) && (! is.null(par$mar))) mar <- par$mar
        par(mfcol = c(Ngraphs, 1), mar=mar, no.readonly=TRUE)
	}  
#     tf <- tframe(tfwindow(x, start=start, end=end))
# would be nice if this could expand tf (tfwindow only truncates - need a
# replacement that expands too.)
     if(length(xlab) == 1) xlab <- rep(xlab, nseries(x))
     for (i in series)
       {if(mode(i)=="character") i <- match(i, names)
	z <-  selectSeries(x, series=i)
        for (d in list(...))
    	   z <- tbind(z, selectSeries(d, series=i)) 
	tfOnePlot(z, tf=tf, start=start, end=end,
	          lty=lty, lwd=lwd, pch=pch, col=col, cex=cex,
		  xlab=xlab[i], ylab=ylab[i], xlim=xlim[[i]], ylim=ylim[[i]])
        if(!is.null(Title) && (i==1) && (is.null(options()$PlotTitles)
                || options()$PlotTitles)) title(main = Title)
	}
    }
  invisible()
 }

tfOnePlot <- function(x, tf=tframe(x), start=tfstart(tf), end=tfend(tf), 
        lty=1:5, lwd=1, pch=NULL, col=1:6, cex=NULL,
        xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, ...)
 {if (!is.tframed(x)) UseMethod("plot")
  else
    {if (!is.null(start)) x <- tfwindow(x, start=start, warn=FALSE)
     if (!is.null(end))   x <- tfwindow(x, end=end, warn=FALSE)
     if(is.null(xlab)) xlab <- ""
     if(is.null(ylab)) ylab <- paste(seriesNames(x), collapse="  ")
     if(is.null(ylim)) ylim <- range(x, na.rm=TRUE)
     tline <- time(x)
     if( inherits(tline, "ts")) tline <- unclass(tline)
     # formerly matplot with tline not a matrix was used, but this does
     # not plot (non-ts) dates as well as plot.
     N <- nseries(x)
     if (1 == N) x <- as.matrix(x)
     else {
        if (length(lty) < N) lty <- rep(lty,length.out=N)
        if (length(lwd) < N) lwd <- rep(lwd,length.out=N)
        if (length(pch) < N) pch <- rep(pch,length.out=N)
        if (length(col) < N) col <- rep(col,length.out=N)
	}
     plot(tline, x[,1], type="l", lty=lty, lwd=lwd, pch=pch, 
        col=col, cex=cex, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, ...)

     if (2 <= N) for (i in 2:N) lines(tline, x[,i],
       type="l", lty=lty[i], lwd=lwd[i], pch=pch[i], col=col[i], ...)
    }
  invisible()
 }



# Note tfprint prints the data. tframePrint  prints the tframe info. 

tfprint <- function(x, ...)  UseMethod("tfprint")

tfprint.default <- function(x,...)
 {xx <- x
  if(1 == nseries(xx)) xx <- matrix(xx, length(xx), 1)
  dimnames(xx) <- list(format(time(tframe(x))), seriesNames(x))
  tframe(xx) <- NULL
  seriesNames(xx) <- NULL
  print(xx, ...)
  invisible(x)
 }



tfwindow <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  UseMethod("tfwindow")

tfwindow.default <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  {# With the default warn=TRUE warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   # kludge
   x <- ts(x, start=tfstart(x), end=tfend(x), frequency=tffrequency(x))
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   y <- window(x, start=start, end=end)
   if (is.matrix(x) && !is.matrix(y) ) y <- matrix(y, length(y), ncol(x))
   y <- tframed(unclass(y), tframe(y))
   seriesNames(y) <- seriesNames(x)
   y
  }


# window a tframe
tfwindow.tframe <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
      tframe(tfwindow(time(x), tf=tf, start=start, end=end, warn=warn))

###############################################

#  tframe  methods   <<<<<<<<<<<<

################################################
is.tframe <- function(x) inherits(x, "tframe")
is.tframed <- function(x) inherits(tframe(x), "tframe")
# above does not distinguish "true" tframed objects since tframe(x) needs
# to try very hard to return tframes from ts and old tsp objects.
#is.Ttframed <- function(x) {!is.null(attr(x, "tframe"))}


tframe <- function(x) UseMethod("tframe")

tframe.default <- function(x){ #extract the tframe
  if(is.null(x)) NULL
  else if(is.tframe(x)) x   
  #else if(is.tframed(x)) tframe(x)  this causes recursion. instead use
  else if (!is.null(attr(x, "tframe"))) attr(x, "tframe") # extractor
  else if (!is.null(tsp(x)))	classed(tsp(x), "tframe") # extractor
  else if(is.vector(x)) classed(c(1,length(x),1), "tframe") # extractor
  else if(is.matrix(x)) classed(c(1,  nrow(x),1), "tframe") # extractor
  else if(is.array(x) ) classed(c(1,dim(x)[1],1), "tframe") # extractor
  #else NULL
  # to catch possible lingering old representations
  else stop("Cannot extract tframe from tframed object")
}
# using classed(tsp(as.ts(x)), "tframe") in the last line above 
# makes too many things into tframes (eg lists)

as.tframed <- function(x) # guarantee x has a tframe 
 {# tframe(x) generates a default
  if (is.tframed(x)) x else tframed(x, tf=tframe(x))
 }
 
as.tframe <- function(...) #constructor
 {#expecting a combination of start, end, frequency, delta, periods,
  #which has enough info to calculate periods. (defaults work for other things.)
  # This is not very generic. The list must define a ts.
  lst <- list(...)
  if(is.null(lst$periods) & (is.null(lst$start) | is.null(lst$end)) ) 
     stop("Must supply periods or start and end.")

  if(is.null(lst$start) & is.null(lst$end)) lst$start <- c(1,1)
  if (! is.null(lst$frequency))   f <- lst$frequency
  else if (! is.null(lst$deltat)) f <- 1/lst$deltat
  else f <- 1
  #more generic date calc. would be nice here
  if (! is.null(lst$periods)) p <- lst$periods
  else p <- 1 + f * (lst$end[1] - lst$start[1]) + (lst$end[2] - lst$start[2])

  # ts seems to want missing values rather than null.
  if (is.null(lst$start)) z <- ts(rep(0,p), end=lst$end,   frequency=f) 
  else                    z <- ts(rep(0,p), start=lst$start, frequency=f)
  tframe(z)
  }
 

"tframe<-" <- function(x, value) 
  {if(is.null(value))
    {attr(x, "tframe") <- NULL
     class(x) <- class(x)[class(x) != "tframed"]
     return(x)
    } 
   else tfSet(value, x) 
  }

tfSet <- function(value, x) UseMethod("tfSet") # dispatch on value

# tfSet.default <- function(value, x) {
#   # It is tempting in the next to try and make a ts if value is from a ts, 
#   #  but that will not work for cases were x does not fit the ts model, so
#   #  that would break  tframe(x) <- tframe(y) 
#   if(!is.tframe(value)) {
#       # do.call does not seem to work when x is passed as NULL 
#       if(is.null(value$start) & is.null(value$end))
#                         stop("Could not determine a tframe from value.")
#       value <- as.tframe(start=value$start, end=value$end, 
#                         frequency=value$frequency, periods=periods(x))
#       }
#   if(! is.tframe(value)) stop("Could not determine a tframe from value.")
#   # next is checking after the fact, but value may just be start and freq
#   #  which is not enough to know periods
#   attr(x, "tframe") <- value
#   if((!is.null(value)) && !checktframeConsistent(tframe(x), x))
#      stop("time frame value in tframe assignment is not consistent with data.")
#   classed(x, c(class(x), "tframed"))
# }

  # It is tempting in the next to try and make a ts if value is from a ts, 
  #  but that will not work for cases were x does not fit the ts model, so
  #  that would break  tframe(x) <- tframe(y) 

tfSet.list <- function(value, x) {
  if(!is.tframe(value)) {
      # do.call does not seem to work when x is passed as NULL 
      if(is.null(value$start) & is.null(value$end))
                        stop("Could not determine a tframe from value.")
      value <- as.tframe(start=value$start, end=value$end, 
                        frequency=value$frequency, periods=periods(x))
      }
  if(! is.tframe(value)) stop("Could not determine a tframe from value.")
  # next is checking after the fact, but value may just be start and freq
  #  which is not enough to know periods
  attr(x, "tframe") <- value
  if((!is.null(value)) && !checktframeConsistent(tframe(x), x))
     stop("time frame value in tframe assignment is not consistent with data.")
  classed(x, c(class(x), "tframed"))
}

tfSet.default <- function(value, x) {
  if(is.numeric(value && (length(value) == 3))) {
      # try to make tsp into a tstframe
      z <- ts(1:periods(x))
      tsp(z) <- value
      value <- tframe(z)
      }
  if(  is.tframed(value)) value <- tframe(value)  
  if(! is.tframe(value))  value <- as.tframe(value)  
  if(! is.tframe(value)) stop("Could not determine a tframe from value.")
  # next is checking after the fact, but value may just be start and freq
  #  which is not enough to know periods
  attr(x, "tframe") <- value
  if((!is.null(value)) && !checktframeConsistent(tframe(x), x))
     stop("time frame value in tframe assignment is not consistent with data.")
  classed(x, c(class(x), "tframed"))
}

# making tframed generic allows tframed.TSdata to specify input and output names

tframed <- function(x, tf=NULL, names = NULL, ...) UseMethod("tframed")

tframed.default <- function(x, tf=NULL, names = NULL, start=NULL, end=NULL, ...)
 {# return x as a tframed object with tframe tf
  if (!is.null(names))  seriesNames(x) <-  names
  if (is.null(tf))
     if ((!is.null(start)) | (!is.null(end))) 
           tf <- as.tframe(start=start, end=end, periods=periods(x), ...)
     else  tf <- tframe(x) # this generates a default
  tframe(x) <- tf
  x
 }


###############################################

#  Generic .tframe methods (these act on the tframe not on the data)

###############################################


#tfprint.tframe <- function(x, ...) UseMethod("tframePrint")
tfprint.tframe <- function(x, ...) UseMethod("print")
#tframePrint <- function(x, ...) UseMethod("tframePrint")

#tframePrint.default <- function(x, digits=NULL, quote=TRUE, prefix="", ...) 
#  {if (! is.tframe(x)) x <- tframe(x)
#   invisible(print(unclass(x), quote=quote, prefix=prefix, ...)) }

print.tframe <- function(x, ...) invisible(print(unclass(x), ...))


tfTruncate.tframe <- function(x, start=NULL, end=NULL)
    {# like window but uses indexes rather than dates 
     if (!is.null(end))   x[2] <- x[1] + (end-1)/x[3]
     if (!is.null(start)) x[1] <- x[1] + (start-1)/x[3]
     x
    }



tfExpand.tframe <- function(x, add.start=0, add.end=0)
    {x[2] <- x[2] + add.end/x[3]
     x[1] <- x[1] - add.start/x[3]
     x
    }


checktframeConsistent <- function(tf, x) UseMethod("checktframeConsistent")

checktframeConsistent.default <- function(tf, x) tfperiods(tf) == periods(x)

testEqualtframes <- function(tf1, tf2) UseMethod("testEqualtframes")

testEqualtframes.default <- function(tf1, tf2) { all(tf1==tf2)}



# Following could be used to do date comparisons like tfstart() < tfend()


earliestStart <- function(x, ...)
    tfstart(append(list(x),list(...))[[earliestStartIndex(x, ...)]])

earliestStartIndex <- function(x, ...) UseMethod("earliestStartIndex")

earliestStartIndex.default <- function(x, ...)
  {tf <- list(tframe(x))
   for (i in list(...)) tf <- append(tf, list(tframe(i)))
   do.call("earliestStartIndex", tf) #dispatch on 1st element of tf
  }

earliestStartIndex.tframe <- function(x, ...) 
    {r <- 1
     fr <- tffrequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] < args[[r]][1]) r <- i
         }           
     r
    }




earliestEnd <- function(x, ...)
    tfend(append(list(x),list(...))[[earliestEndIndex(x, ...)]])

earliestEndIndex <- function(x, ...) UseMethod("earliestEndIndex")

earliestEndIndex.default <- function(x, ...)
  {tf <- list(tframe(x))
   for (i in list(...)) tf <- append(tf, list(tframe(i)))
   do.call("earliestEndIndex", tf) #dispatch on 1st element of tf
  }

earliestEndIndex.tframe <- function(x, ...) 
    {r <- 1
     fr <- tffrequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] < args[[r]][2]) r <- i
         }           
     r
    }



latestStart <- function(x, ...)
    tfstart(append(list(x),list(...))[[latestStartIndex(x, ...)]])

latestStartIndex <- function(x, ...) UseMethod("latestStartIndex")

latestStartIndex.default <- function(x, ...)
  {tf <- list(tframe(x))
   for (i in list(...)) tf <- append(tf, list(tframe(i)))
   do.call("latestStartIndex", tf)
  }


latestStartIndex.tframe <- function(x, ...) 
    {r <- 1
     fr <- tffrequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] > args[[r]][1]) r <- i
         }           
     r
    }



latestEnd <- function(x, ...)
    tfend(append(list(x),list(...))[[latestEndIndex(x, ...)]])

latestEndIndex <- function(x, ...) UseMethod("latestEndIndex")

latestEndIndex.default <- function(x, ...)
  {tf <- list(tframe(x))
   for (i in list(...)) tf <- append(tf, list(tframe(i)))
   do.call("latestEndIndex", tf)
  }

latestEndIndex.tframe <- function(x, ...) 
    {r <- 1
     fr <- tffrequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] > args[[r]][2]) r <- i
         }           
     r
    }




###############################################

#  stamped specific methods   <<<<<<<<<<<<
#  stamped class TS have a date/time stamp associated with each time point
################################################

#checktframeConsistent.stamped <- function(tf, x)
#  {periods(x) == periods(tf)}

testEqualtframes.stamped <- function(tf1, tf2)
  {all(tf1$stamp == tf2$stamp)}

tfperiods.stamped <- function(x) length(tframe(x))

###############################################

testEqual <- function(obj1, obj2, fuzz = 0) UseMethod("testEqual")

testEqual.default <- function(obj1, obj2, fuzz=1e-16) 
  {if      (is.null(obj1)) is.null(obj2)
   else if (is.array(obj1)) testEqual.array(obj1, obj2, fuzz=fuzz)
   else if (is.numeric(obj1)) testEqual.numeric(obj1, obj2, fuzz=fuzz)
   else if (is.list(obj1)) testEqual.list(obj1, obj2, fuzz=fuzz)
   else is.logical(all.equal(obj1, obj2, tolerance=fuzz))
  }

testEqual.array <- function(obj1, obj2, fuzz=1e-16) 
  {if(!is.array(obj2))                     r <-FALSE
   else if (any(dim(obj1) != dim(obj2)))   r <- FALSE
   else if ("character" == mode(obj1))     r <- all(obj1 == obj2)
   else if ("numeric" == mode(obj1))
              r <- testEqual.numeric(obj1, obj2, fuzz=fuzz)
   else stop(paste("matrix of mode ", mode(obj1), " not testable."))
   if (is.na(r))  r <- FALSE
    r
  }

testEqual.matrix <- testEqual.array

testEqual.numeric <- function(obj1, obj2, fuzz=1e-16) 
  {r <- all(is.infinite(obj1) == is.infinite(obj2))
   if (r) 
          {nna <- !is.na(c(obj1))
           r <- fuzz >= max(abs(c(obj1)[nna] - c(obj2)[nna]))
          }
   if (is.na(r))  r <- FALSE
   r
  }

testEqual.list <- function(obj1, obj2, fuzz=1e-16) 
  {r <- length(obj1) == length(obj2)
   if (r) for (i in seq(length(obj1)))
        {if(r) r <- testEqual(obj1[[i]], obj2[[i]], fuzz=fuzz) }
   r
  }

#if (!exists("lag")) lag <- function(x, ...) UseMethod("lag")

#if (!exists("lag.default"))  lag.default <- function(x, ...) {stop("no lag function") }



###############################################

# Time dimension methods for data manipulation

###############################################



splice <- function(mat1, mat2, ...) UseMethod("splice")

splice.default <- function(mat1, mat2, ...)
{#  (... further arguments, currently disregarded)
 # splice together 2 time series matrices. If data  is provided in both for
 #  a given period then mat1 takes priority.
 # The result starts at the earlier of mat1 and mat2 and ends at the later.
 # dimnames are taken from mat1.
 # The frequencies should be the same.
 if (is.null(mat1)) return(mat2)
 if (is.null(mat2)) return(mat1)
 freq <- tffrequency(mat1)
 if (freq != tffrequency(mat2)) stop("frequencies must be the same.")
 p <- dim(mat1)[2]
 if (p != dim(mat2)[2])   stop("number of series must be the same.")
 fr <- c(freq,1)
 st <- min(fr %*% tfstart(mat1), fr %*% tfstart(mat2))
 strt <- c(st %/% freq, st %% freq)
 en <- max(fr %*% tfend(mat1), fr%*% tfend(mat2))
 r1 <-r2 <-tframed(matrix(NA, 1+en-st, p), list(start=strt, frequency=freq))
 r1[c((fr %*% tfstart(mat1))-st) + 1:dim(mat1)[1],] <- mat1
 r2[c((fr %*% tfstart(mat2))-st) + 1:dim(mat2)[1],] <- mat2
 na <- is.na(r1)
 r1[na] <- r2[na] # put mat2 only in na locations of mat1
 dimnames(r1)<-list(round(time(r1),digits=3),dimnames(mat1)[[2]])
 r1 <- tframed(r1, list(start=earliestStart(mat1,mat2), 
                        end =latestEnd(mat1,mat2), frequency=freq))
 r1
}


tfTruncate <- function(x, start=NULL, end=NULL) UseMethod("tfTruncate")
  # similar to window but start and end specify periods relative to the 
  #   beginning (eg x[start:end] for a vector).
  #   NULL means no truncation.


tfTruncate.default <- function(x, start=NULL, end=NULL)
    {tf <- tfTruncate(tframe(x), start, end)
     if (is.null(start)) start <- 1
     if (is.matrix(x)) 
        {if (is.null(end)) end <- dim(x)[1]
         z <- x[start:end,,drop=FALSE]
        }
     else 
        {if (is.null(end)) end <- length(x)
         z <- x[start:end]
        }
     tframe(z) <- tf
     z
    }

tfExpand <- function(x, add.start=0, add.end=0) UseMethod("tfExpand")
  # expand (a tframe) by add.start periods on the beginning
  # and add.end periods on the end

tfExpand.default <- function(x, add.start=0, add.end=0)
    {tf <- tfExpand(tframe(x), add.start=add.start, add.end=add.end)
     selectSeries(tbind(x,time(tf)), series=1)
    }


trimNA <- function(x, startNAs= TRUE, endNAs= TRUE) UseMethod("trimNA") 

trimNA.default <- function(x, startNAs= TRUE, endNAs= TRUE)
{# trim NAs from the ends of a ts matrix or vector.
 # (Observations for all series are dropped in a given period if any 
 #  one contains an NA in that period.)
 # if startNAs=F then beginning NAs are not trimmed.
 # If endNAs=F   then ending NAs are not trimmed.
 sample <- ! if (is.matrix(x)) apply(is.na(x),1, any) else is.na(x)
 if (!any(sample)) warning("data is empty after triming NAs.")
 s <- if (startNAs) min(time(x)[sample]) else tfstart(x)
 e <- if (endNAs)   max(time(x)[sample]) else tfend(x)
 tfwindow(x, start=s, end=e, warn=FALSE)
}



diffLog <- function(obj, lag = 1, base = exp(1),
              names=paste("diff of log of ", seriesNames(obj))) 
   UseMethod("diffLog")
 
diffLog.default <- function(obj, lag = 1, base = exp(1),
              names=paste("diff of log of ", seriesNames(obj)))
{#Calculate the difference from lag periods prior for log of data.
 obj <- diff(log(obj, base = base), lag = lag)
 if(is.null(options()$ModSeriesNames) || options()$ModSeriesNames)
        seriesNames(obj) <- names
 obj
}


aggregate.tframed <- function (x, ...)
   {tf <- tframe(x)
    nm <- seriesNames(x)
    # this is assuming tf is actual a ts tframe
    r <- aggregate(ts(unclass(x), start=tf[1], end=tf[2], frequency=tf[3]), ...)
    tframed(r, tf=tframe(r), names=nm)
   }

ytoypc <- function(obj, names=paste("y to y %ch", seriesNames(obj))) 
   UseMethod("ytoypc")
 
ytoypc.default <- function (obj, names=paste("y to y %ch", seriesNames(obj)) ){
   obj <- percentChange(obj, lag = tffrequency(obj))
   if(is.null(options()$ModSeriesNames) || options()$ModSeriesNames)
        seriesNames(obj) <- names
   obj
}


percentChange <- function(obj, ...) UseMethod("percentChange")

percentChange.default <- function(obj, base=NULL, lag=1, 
      cumulate=FALSE, e=FALSE, ...)
{#  (... further arguments, currently disregarded)
   cls <- class(obj)
   # note next has to be applied to a shorter object in the end
   if (is.tframed(obj)) tf <- list(end=tfend(obj), frequency=tffrequency(obj))
   else tf <- NULL
   if (is.null(dim(obj)))
     {vec <- TRUE
      obj <- matrix(obj, length(obj),1)
     }
   else vec <- FALSE
   mm <- rbind(base,obj)
   if (any(cumulate))
          mm[,cumulate] <-apply(mm[,cumulate,drop=FALSE],2,cumsum)
   if (any(e)) mm[,e] <- exp(mm[,e,drop=FALSE])
   N <- dim(mm)[1]
   pchange <-100*(mm[(lag+1):N,,drop=FALSE] - 
                    mm[1:(N-lag),,drop=FALSE])/mm[1:(N-lag),,drop=FALSE]
   if (vec) pchange <- pchange[,1]
   #class(pchange) <- cls[cls != "tframed"]
   if (!is.null(tf)) tframed(pchange, tf) else pchange
}

annualizedGrowth <- function(obj, ...) UseMethod("annualizedGrowth")

annualizedGrowth.default <- function(obj, lag=1, freqLagRatio=frequency(obj)/lag,
        names=paste("Annual Growth of", seriesNames(obj)), ...) {
  r <- 100*((obj/tfL(obj, p= lag))^freqLagRatio - 1)
  if(is.null(options()$ModSeriesNames) || options()$ModSeriesNames)
        seriesNames(r) <- names
  r
  }

###############################################

# Non-time dimension methods

###############################################


nseries <- function(x) UseMethod("nseries") 
nseries.default <- function(x)   if (is.null(x)) 0 else NCOL(x)

   

 seriesNames <- function(x)       UseMethod("seriesNames")
"seriesNames<-" <- function(x, value)UseMethod("seriesNames<-")

 seriesNames.default <- function(x) {
    if (is.null(x)) return(NULL)
    names <- attr(x, "seriesNames")
    if (is.null(names)) names <- dimnames(x)[[2]]
    if (is.null(names)) names <- paste("Series", seq(ncol(x)))
    names
    }

"seriesNames<-.default" <- function(x, value) {
   if (!is.null(value)) {
      if (mode(value) != "character") value <- seriesNames(value)
      if (length(value) != nseries(x))
         stop("length of names (",length(value),
	      ") does not match number of series(",nseries(x),").")
      }
   attr(x,"seriesNames")<-value
   x
   }



selectSeries <- function(x, series=seqN(nseries(x))) UseMethod("selectSeries")

selectSeries.default <- function(x, series=seqN(nseries(x))) {
  names <- seriesNames(x)
  if (is.character(series)) series <- match(names,series, nomatch=0) > 0
  if(all(0==series) | is.null(series)) r <- NULL
  else if (!is.matrix(x)) r <- x  # vector case
  else {
#    r <- classed(tframed(x[, series, drop = FALSE], tframe(x)), class(x))# reconstructor
#   tframe assignment cannot guarantee that the object has the right structure
#   for a class, so above can give a deformed object in the class.
    r <- tframed(x[, series, drop = FALSE], tframe(x))
    seriesNames(r) <- names[series]
    }
  r
  }


tbind <- function(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
   UseMethod("tbind")


tbind.default <- function (x, ..., pad.start = TRUE, pad.end = TRUE, warn = TRUE) 
{
    if (is.null(x)) {
	#stop("first argument cannot be NULL.")
        r <- list(...)
	if(1 != length(r))
	    stop("If first argument is NULL then only one other series should be supplied.")
	return(r[[1]])
	}
    fr <- tffrequency(x)
    for (i in list(...)) {
        if (!is.null(i) && (fr != tffrequency(i))) 
            stop("frequencies must be the same.")
    }
    fr <- c(fr, 1)
    st <- fr %*% tfstart(x)
    for (i in list(...)) if (!is.null(i)) 
        st <- min(st, fr %*% tfstart(i))
    en <- fr %*% tfend(x)
    for (i in list(...)) if (!is.null(i)) 
        en <- max(en, fr %*% tfend(i))
    r <- NULL
    sn <- NULL
    nm <- attr(x, "names")
    attr(x, "names") <- NULL
    for (z in append(list(x), list(...))) {
        if (!is.null(z)) {
            if (is.matrix(z)) {
                if (st == (fr %*% tfstart(z))) 
                  before <- NULL
                else before <- matrix(NA, (fr %*% tfstart(z)) - 
                  st, dim(z)[2])
                if (en == (fr %*% tfend(z))) 
                  aft <- NULL
                else aft <- matrix(NA, en - (fr %*% tfend(z)), 
                  dim(z)[2])
                r <- cbind(r, rbind(before, z, aft))
            }
            else {
                if (st == (fr %*% tfstart(z))) 
                  before <- NULL
                else before <- rep(NA, (fr %*% tfstart(z)) - 
                  st)
                if (en == (fr %*% tfend(z))) 
                  aft <- NULL
                else aft <- rep(NA, en - (fr %*% tfend(z)))
                r <- cbind(r, c(before, z, aft))
            }
            sn <- c(sn, seriesNames(z))
        }
    }
    if (!is.null(nm)) 
        dimnames(r) <- list(nm, NULL)
    if (length(sn) == ncol(r)) 
        seriesNames(r) <- sn
    r <- tframed(r, list(start = c((st - 1)%/%fr[1], 1 + (st - 
        1)%%fr[1]), frequency = fr[1]))
    if (!(pad.start & pad.end)) 
        r <- trimNA(r, startNAs = !pad.start, endNAs = !pad.end)
    if (is.null(r)) 
        warning("intersection is NULL")
    r
}

############################################################################

#   miscellaneous utilities  <<<<<<<<<<
#   (Useful utilities not strictly part of tframe)

############################################################################

addDate <- function(date, periods, freq)
  {if (is.null(periods)) periods <- 0
   c(date[1]+(date[2]+periods-1)%/%freq, 1+(date[2]+periods-1)%%freq)
  }


tsScan <- function(file="", skip=1, nseries=1, sep=",", 
           na.strings=c("NA", "NC", "ND"), ...)
   {# all args passed to scan. Expects a file with (default one) title line 
    # to skip and data in three columns (default separated with commas):
    #   year, period, data[;1], data[;2], ..., data[;nseries]
    # and builds a ts with freq set to max(period)
    z <- scan(file=file, skip=skip, what=as.list(c(seq(2), double(nseries))),
              sep=sep, na.strings=na.strings, ...)
    zz <- NULL
     for  (i in 1:nseries) zz <- cbind(zz, z[[2+i]])
    ts(zz, start=c(z[[1]][1], z[[2]][1]), frequency=max(z[[2]]))
    }

tsWrite <- function(x, file="data", header=TRUE, sep=",", digits=16)
   {# all args passed to scan. Expects a ts or mts.
    # write file with (default one) title line. 
    # then data in three columns (efault separated with commas):
    #   year, period, data[;1], data[;2], ...
    if (header) write(paste("year", "period", 
                   paste(seriesNames(x), collapse=sep),sep=sep), file=file)
    yr  <- floor(time(x))
    pr  <- 1+ (time(x) %% 1) * frequency(x)
    dg <- options(digits=digits)
    on.exit(options(dg))
    x <- as.matrix(x)
    write(t(cbind(yr, pr, x)), file=file, ncolumns = 2 + ncol(x), sep=sep, append=header)
    }


