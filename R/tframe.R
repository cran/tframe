#   2000/03/21 14:41:54 

#   Copyright 1993, 1994, 1995, 1996  Bank of Canada.
#   Copyright 1997 (June), Paul Gilbert.
#   Copyright 1997 (Aug.), Bank of Canada.
#   Copyright 1998, 1999, 2000   Bank of Canada.

#   The user of this software has the right to use, reproduce and distribute it.
#   Bank of Canada and Paul Gilbert make no warranties with respect to the 
#   software or its fitness for any particular purpose. 
#   The software is distributed by the Bank of Canada and by Paul Gilbert 
#   solely on an "as is" basis. By using the software, user agrees to accept 
#   the entire risk of using this software.

################################################################################


#   2000/04/20 14:50:55  

###########################################################################

#  Internal utilities to limit dependence on class/oldclass

if (is.R())
  { tfclass    <- .Alias(class)
   "tfclass<-" <- .Alias(get("class<-"))
   require("syskern",  warn.conflicts=F)  # this may not be necessary
  } else
if (is.S())
  { tfclass     <- class
   "tfclass<-" <- function(x, value){ class(x) <- value ; x }
  }

#if (is.S5())
#  { tfclass     <- oldClass
#   "tfclass<-" <- function(x, value){ oldClass(x) <- value ; x }
#  }


classed <- function(x, cls) {tfclass(x) <- cls; x}

# next should use settf to dispatch on the class method for tf not the 
#  class for x. Setting the class of x first really messes up it dispatches 
#   without having the appropriate attributes of the class.
#tfclassed <- function(x, cls, tf) {  tframe(x) <- tf; tfclass(x) <- cls; x}

###########################################################################

#  Misc. internal utilities

# Use this with "for (i in seq(length=m) )" as m==0 returns NULL and for does no loops
seqN <- function(N) {if (0==length(N)) NULL else if (N<=0) NULL else seq(N)}

###########################################################################



###########################################################################

# tframe classes and methods       <<<<<<<<<<<<

###########################################################################
   









                






                 

###############################################

#  generic methods  and defaults <<<<<<<<<<<<

################################################

# start, end, frequency, time and window
# are already generic functions in S with a default 
# method which works for vectors and matrices and data with a tsp attribute.


# The functions diff and tsmatrix are not (yet) generic function is S. The
# existing functions are assigned as defaults and generic ones are defined.

periods <- function(x, ...)
 {# the length in time of the sequence (number of observations)
    UseMethod("periods")
 }

periods.default <- function(x)
  {if (is.array(x)) return(dim(x)[1])
   else return(length(x))
  }

periods.tsp <- periods.default 

#  tfplot and tfprint below provide generic methods for ploting and printing
#  tf time series objects. Plot methods will probably to some processing
#  and eventually call tfplot.default.

tfplot <- function(obj, ...)  UseMethod("tfplot")

tfplot.default <- function(obj, xlab=NULL, ylab=NULL,
                           start.=NULL, end.=NULL, ...)
 {if (!is.tframed(obj)) UseMethod("tfplot")
  else
    {if (!is.null(start.)) obj <- tfwindow(obj, start. = start.)
     if (!is.null(end.))   obj <- tfwindow(obj, end.   = end.)
     tline <- time(obj)
     if(is.null(xlab)) xlab <- ""
     if(is.null(ylab)) ylab <- paste(series.names(obj), collapse="  ")
     matplot(tline, obj, type="l", xlab=xlab, ylab=ylab, ...)
    }
  invisible()
 }

# Note tfprint prints the data. tfprint.tframe  prints the tframe info. 

tfprint <- function(x, ...)  UseMethod("tfprint")

tfprint.default <- function(x,...)
 {dimnames(x) <- list(format(time(tframe(x))), series.names(x))
  tframe(x) <- NULL
  series.names(x) <- NULL
  print(x, ...)
  invisible(x)
 }



tfwindow <- function(x, ...)  UseMethod("tfwindow")

tfwindow.default <- function(x, start.=NULL, end.=NULL, warn=T)
  {# this provides a convenient way to support warn and correct for bugs
   # in some versions of window().
   # if (is.null(start.)) start. <- start(x)
   # if (is.null(end.))   end.   <- end(x)
   # With the default warn=T warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   y <- window(x, start=start., end=end.)
   if (is.matrix(x) && !is.matrix(y) )
      y <- tframed(matrix(y, length(y), ncol(x)), tframe(y))
   series.names(y) <- series.names(x)
   y
  }

###############################################

#  tframe  methods   <<<<<<<<<<<<

################################################
is.tframe  <- function(tf) inherits(tf, "tframe")
is.tframed <- function (x) inherits(tframe(x), "tframe")

tframe <-function(x) {UseMethod("tframe") } #extract the tframe

tframe.default <- function(x){
	if(is.null(x)) NULL 
	else if (!is.null(attr(x, "tframe"))) attr(x, "tframe") # constructor
	else if (!is.null(tsp(x)))    classed(tsp(x), "tframe") # constructor
	else if(is.array(x) && !is.matrix(x)) 
		classed(tsp(as.ts(seq(dim(x)[1]))), "tframe") # constructor
	else 	classed(tsp(as.ts(x)), "tframe") # constructor
}

# Following switches dispatch to class of tf value rather than class of x
#    except in the case of null assignment.

"tframe<-" <-function(x, value)
 {if(is.null(value)) UseMethod("tframe<-")
  else settf(value,x)
 }

"tframe<-.default" <-function(x, value) {tsp(x) <- value; x}

settf <- function(value, x) {UseMethod("settf") }

settf.default     <- function(value, x)
{if (!is.consistent.tframe(value, x))
    stop("time frame value in tframe assignment is not consistent with data.")
 tsp(x) <- value
 x
}


tframed  <- function (x, ...) {UseMethod("tframed") }

tframed.default  <- function (x, tf=NULL, names = NULL) 
{# return x as a tframed object with tframe tf
 # If ts is not a tframe but a list then ts() is attempted. This is not
 #     really the way tframed is suppose to be used, but makes backward 
 #     compatability easier.
    if (!is.null(names))  series.names(x) <-  names
    if (is.null(tf)) tf <- tframe(x)
    if (is.tframe(tf)) x <- settf(tf, x)
    else if (is.list(tf))
       {if( is.null(tf$start) & is.null(tf$end) )
           stop("tf must be a tframe or a list of arguments for ts().")
        x <- do.call("ts", append(list(x), tf))
       }
    x
}



###############################################

#  Generic .tframe methods (these act on the tframe not on the data)

###############################################


tfprint.tframe <-function(x, digits=NULL, quote=T, prefix="", ...) 
   UseMethod("tfprint.tframe")

tfprint.tframe.default <-function(x, digits=NULL, quote=T, prefix="", ...) 
   invisible(print(unclass(x), quote=quote, prefix=prefix, ...))
   # digits=digits, seems to cause problems ?



start.tframe <- function(tf)UseMethod("start.tframe")
end.tframe   <- function(tf)UseMethod("end.tframe")

# periods should give the number of data points in the time direction.
periods.tframe <- function(tf)UseMethod("periods.tframe")

# frequency is less essential and may not always make sense.
frequency.tframe <-function(tf)UseMethod("frequency.tframe")

time.tframe <- function(tf)UseMethod("time.tframe")

truncate.tframe <-function(tf, start=NULL, end=NULL)
    {#NULL means no truncation.
     UseMethod("truncate.tframe")
    }

expand.tframe <-function(tf, add.start=0, add.end=0)
     UseMethod("expand.tframe")



is.consistent.tframe <-function(tf, x) UseMethod("is.consistent.tframe")

is.consistent.tframe.default <-function(tf, x)
   {periods.tframe(tf) == periods(x)}

test.equal.tframe <-function(tf1, tf2) UseMethod("test.equal.tframe")

test.equal.tframe.default <-function(tf1, tf2) { all(tf1==tf2)}

# Following could be used to do date comparisons like start() < end()

earliest.start.index.tframe <-function(x, ...)
    UseMethod("earliest.start.index.tframe")

earliest.start.tframe <-function(x, ...)
    append(list(x),list(...))[[earliest.start.index.tframe(x, ...)]]

earliest.end.index.tframe <-function(x, ...)
    UseMethod("earliest.end.index.tframe")

earliest.end.tframe <-function(x, ...)
    append(list(x),list(...))[[earliest.end.index.tframe(x, ...)]]

latest.start.index.tframe <-function(x, ...)
    UseMethod("latest.start.index.tframe")

latest.start.tframe <-function(x, ...)
    append(list(x),list(...))[[latest.start.index.tframe(x, ...)]]

latest.end.index.tframe <-function(x, ...)
    UseMethod("latest.end.index.tframe")

latest.end.tframe <-function(x, ...)
    append(list(x),list(...))[[latest.end.index.tframe(x, ...)]]


###############################################

#  default .tframe methods   <<<<<<<<<<<<

################################################

start.tframe.default <-function(tf) {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.default <-function(tf) {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.default <-function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.default <-function(tf) {tf[3]}

time.tframe.default <-function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

truncate.tframe.default <-function(tf, start=NULL, end=NULL) 
    {# like window but uses indexes rather than dates
     if (!is.null(end))   tf[2] <- tf[1] + (end-1)/tf[3]
     if (!is.null(start)) tf[1] <- tf[1] + (start-1)/tf[3]
     tf
    }

expand.tframe.default <-function(tf, add.start=0, add.end=0) 
    {tf[2] <- tf[2] + add.end/tf[3]
     tf[1] <- tf[1] - add.start/tf[3]
     tf
    }


earliest.start.index.tframe.default <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] < args[[r]][1]) r <- i
         }           
     r
    }

earliest.end.index.tframe.default <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] < args[[r]][2]) r <- i
         }           
     r
    }

latest.start.index.tframe.default <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] > args[[r]][1]) r <- i
         }           
     r
    }

latest.end.index.tframe.default <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] > args[[r]][2]) r <- i
         }           
     r
    }


###############################################

#   .ts and .tframe.tstframe methods

###############################################

#"tframe.tframe<-.ts"      <- function(x, value) {tsp(x) <- value; x}

tframe.ts <- function(x){classed(tsp(x), c("tstframe", "tframe"))} # constructor

"tframe<-.ts"          <- function(x, value) {tsp(x) <- value; x}

#settf.default works for .ts

start.tframe.tstframe <-function(tf)
   {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tstframe <-function(tf)
   {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tstframe <-function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tstframe <-function(tf) {tf[3]}

time.tframe.tstframe <-function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}



###############################################

#   .tf and .tframe.tftframe methods

# these provide .ts or tsp like methods for more general arrays with time as the
#   first dimension. It may be possible extend to more general structures.

###############################################

tframe.tf   <- function(x) {attr(x, "tf") }

"tframe<-.tf"     <- function(x, value)
  {if (!is.null(value)) return(settf.tftframe(value, x))
   else 
     {attr(x, "tf") <- NULL
      if (!is.null(class(x))) class(x) <- class(x)[! class(x) == "tf"]
     }
   x
  }

#tf <- function(x, tfr=NULL, ...){stop("defunct. Use tframed.")}
#tf <- function(x, tfr=NULL, ...)
# {#  tfr should be a tframe or other args are passed to ts.
#  # the class is set to "tftframe" so objects are interchangable R to S
#  if (is.null(tfr)) 
#      {tfr <- tframe(do.call("ts", append(list(seq(periods(x))), list(...))))
#       cls <- class(tfr)
#       cls[cls == "tstframe"] <- "tftframe"
#       class(tfr) <- cls
#      }
#  if (!is.tframe(tfr)) stop("tfr must be a tframe")
#  settf.tftframe(tfr, x)
# }


settf.tftframe  <- function(value, x)
{#class(value) <- c("tftframe", "tframe")
 if (!is.consistent.tframe(value, x))
    stop("time frame value in tframe assignment is not consistent with data.")
 attr(x, "tf") <- value
 classed(x, "tf")  # constructor (settf.tftframe)
}


start.tf     <- function(x) {start(tframe(x))}
end.tf       <- function(x) {end(tframe(x))}
periods.tf   <- function(x) {periods(tframe(x))}
frequency.tf <- function(x) {frequency(tframe(x))}
time.tf      <- function(x) {time(tframe(x))}

start.tframe.tftframe <-function(tf)
   {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tftframe <-function(tf)
   {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tftframe <-function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tftframe <-function(tf) {tf[3]}

time.tframe.tftframe <-function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

tfwindow.tf <- function  (x, start=NULL, end=NULL, warn=T, eps=.Options$ts.eps) 
  {# this needs work
   tfwindow(ts(x, start=start(x), frequency=frequency(x)),
             start=start, end=end, warn=warn)
  }

###############################################

#   .TSdata methods (for DSE)

###############################################


"tframe<-.TSdata" <-function(x, value)
 {if (0 != input.dimension(x)) tframe( input.data(x)) <- value
  if (0 !=output.dimension(x)) tframe(output.data(x)) <- value
  x
 }



###############################################

#   .rts and .tframe.rtstframe methods

###############################################



"tframe<-.rts"        <-function(x, value) {rts(x) <- value; x}
"tframe.tframe<-.rts" <-function(value, x)
{if (!is.consistent.tframe(value, x))
    stop("time frame value in tframe assignment is not consistent with data.")
 rts(x) <- value
 x
}

###############################################

# add cts its

###############################################


###############################################

#  stamped specific methods   <<<<<<<<<<<<
#  stamped class TS have a date/time stamp associated with each time point
################################################
is.consistent.tframe.stamped <-function(tf, x)
  {periods(x) == periods(tf)}

test.equal.tframe.stamped <-function(tf1, tf2)
  {all(tf1$stamp == tf2$stamp)}

periods.tframe.stamped <- function(x)length(tframe(x))



###############################################



test.equal<- function(obj1, obj2, ...) UseMethod("test.equal")

 
test.equal.default <- function (obj1, obj2, fuzz=1e-16) 
  {if      (is.null(obj1)) is.null(obj2)
   else if (is.array(obj1)) test.equal.array(obj1, obj2, fuzz=fuzz)
   else if (is.numeric(obj1)) test.equal.numeric(obj1, obj2, fuzz=fuzz)
   else if (is.list(obj1)) test.equal.list(obj1, obj2, fuzz=fuzz)
   else is.logical(all.equal(obj1, obj2, tolerance=fuzz))
  }

test.equal.array <- function (obj1, obj2, fuzz=1e-16) 
  {if(!is.array(obj2))                     r <-F
   else if (any(dim(obj1) != dim(obj2)))   r <- F
   else if ("character" == mode(obj1))     r <- all(obj1 == obj2)
   else if ("numeric" == mode(obj1))
              r <- test.equal.numeric(obj1, obj2, fuzz=fuzz)
   else stop(paste("matrix of mode ", mode(obj1), " not testable."))
   if (is.na(r))  r <- F
    r
  }

test.equal.matrix <- test.equal.array

test.equal.numeric <- function (obj1, obj2, fuzz=1e-16) 
  {r <- all(is.infinite(obj1) == is.infinite(obj2))
   if (r) 
          {nna <- !is.na(c(obj1))
           r <- fuzz >= max(abs(c(obj1)[nna] - c(obj2)[nna]))
          }
   if (is.na(r))  r <- F
   r
  }

test.equal.list <- function (obj1, obj2, fuzz=1e-16) 
  {r <- length(obj1) == length(obj2)
   if (r) for (i in seq(length(obj1)))
        {if(r) r <- test.equal(obj1[[i]], obj2[[i]], fuzz=fuzz) }
   r
  }

if (!exists("lag")) lag <- function(x, ...) { UseMethod("lag") }

lag.tframe  <- function(x,...)UseMethod("lag.tframe") 

if (!exists("lag"))  lag.default <- function(x, ...) {stop("no lag function") }




splice <- function(obj1, obj2, ...) UseMethod("splice")

splice.default <-function(mat1, mat2)
{# splice together 2 time series matrices. If data  is provided in both for
 #  a given period then mat1 takes priority.
 # The result starts at the earlier of mat1 and mat2 and ends at the later.
 # dimnames are taken from mat1.
 # The frequencies should be the same.
 if (is.null(mat1)) return(mat2)
 if (is.null(mat2)) return(mat1)
 freq <- frequency(mat1)
 if (freq != frequency(mat2)) stop("frequencies must be the same.")
 p <- dim(mat1)[2]
 if (p != dim(mat2)[2])   stop("number of series must be the same.")
 fr <- c(freq,1)
 st <- min(fr %*% start(mat1), fr %*% start(mat2))
 strt <- c(st %/% freq, st %% freq)
 en <- max(fr %*% end(mat1), fr%*% end(mat2))
 r1 <-r2 <-tframed(matrix(NA, 1+en-st, p), list(start=strt, frequency=freq))
 r1[c((fr %*% start(mat1))-st) + 1:dim(mat1)[1],] <- mat1
 r2[c((fr %*% start(mat2))-st) + 1:dim(mat2)[1],] <- mat2
 na <- is.na(r1)
 r1[na] <- r2[na] # put mat2 only in na locations of mat1
 dimnames(r1)<-list(round(time(r1),digits=3),dimnames(mat1)[[2]])
 r1 <- tframed(r1, list(start=earliest.start(mat1,mat2), 
                        end =latest.end(mat1,mat2), frequency=freq))
 r1
}


if( !exists("tsmatrix.default"))  
  {if(exists("tsmatrix")) tsmatrix.default <- tsmatrix 
   else tsmatrix.default <- function(x, ...) 
            {tbind(x, ..., pad.start=F, pad.end=F) }
  }

tsmatrix <- function(x, ...)
 {# the default tsmatrix messes up because it gets some time info. (from
  #  start or end) but not tsp info.
  if (is.tframed(x)) tbind(x, ..., pad.start=F, pad.end=F)
  else 
    {#warning("Using tsmatrix which should be defunct. Consider using tbind and tframe methods.")       
     tsmatrix.default(x,  ...)
    }
 }


truncate <- function(x, start=NULL, end=NULL)
 {# similar to window but start and end specify periods relative to the 
  #   beginning (eg x[start:end] for a vector).
  #   NULL means no truncation.
  UseMethod("truncate")
 }

truncate.default <-function(x, start=NULL, end=NULL)
    {tf <- truncate.tframe(tframe(x), start, end)
     if (is.null(start)) start <- 1
     if (is.matrix(x)) 
        {if (is.null(end)) end <- dim(x)[1]
         z <- x[start:end,,drop=F]
        }
     else 
        {if (is.null(end)) end <- length(x)
         z <- x[start:end]
        }
     tframe(z) <- tf
     z
    }

expand <- function(x, add.start=0, add.end=0)
 {# expand (a tframe) by add.start periods on the beginning
  # and add.end periods on the end
  UseMethod("expand")
 }

expand.default <-function(x, start=NULL, end=NULL)
    {tf <- expand.tframe(tframe(x), start, end)
     select.series(tbind(x,time(tf)), series=1)
    }


earliest.start <-function(x, ...)
    start(append(list(x),list(...))[[earliest.start.index(x, ...)]])

earliest.start.index <-function(x, ...)
  {if (is.tframe(x)) UseMethod("earliest.start.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("earliest.start.index.tframe", tf)
     }
   r
  }

earliest.end <-function(x, ...)
    end(append(list(x),list(...))[[earliest.end.index(x, ...)]])

earliest.end.index <-function(x, ...)
  {if (is.tframe(x)) UseMethod("earliest.end.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("earliest.end.index.tframe", tf)
     }
   r
  }

latest.start <-function(x, ...)
    start(append(list(x),list(...))[[latest.start.index(x, ...)]])

latest.start.index <-function(x, ...)
  {if (is.tframe(x)) UseMethod("latest.start.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("latest.start.index.tframe", tf)
     }
   r
  }

latest.end <-function(x, ...)
    end(append(list(x),list(...))[[latest.end.index(x, ...)]])

latest.end.index <-function(x, ...)
  {if (is.tframe(x)) UseMethod("latest.end.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("latest.end.index.tframe", tf)
     }
   r
  }



trim.na <- function(obj, ...) UseMethod("trim.na") 

trim.na.default <-function(x, start.=T, end.=T)
{# trim NAs from the ends of a ts matrix.
 # (Observations for all series are dropped in a given period if any 
 #  one contains an NA in that period.)
 # if start.=F then beginning NAs are not trimmed.
 # If end.=F   then ending NAs are not trimmed.
 sample <- ! apply(is.na(x),1, any)
 if (!any(sample)) warning("data is empty after triming NAs.")
 if (start.) s <-min(time(x)[sample])
 else       s <-start(x)
 if (end.)   e <-max(time(x)[sample])
 else       e <-end(x)
 tfwindow(x,start=s, end=e, warn=F)
}


###############################################

# Non-time dimension methods

###############################################



nseries <- function (x) {UseMethod("nseries")} 
nseries.default  <- function(x)  {ncol(x)} 

   

 series.names     <- function(x)       UseMethod("series.names")
"series.names<-"  <- function(x, value)UseMethod("series.names<-")

 series.names.default    <-function(x)
   {if (is.null(x)) return(NULL)
    names <- attr(x, "series.names")
    if (is.null(names)) names <- dimnames(x)[[2]]
    if (is.null(names)) names <- paste("Series", seq(ncol(x)))
    names
   }

"series.names<-.default" <-function(x, value){attr(x,"series.names")<-value; x}



select.series <- function(x, ...)  UseMethod("select.series")

select.series.default <- function(x, series=seq(ncol(x))) {
  names <- series.names(x)
  if (is.character(series)) series <- match(names,series, nomatch=0)
  if(all(0==series) | is.null(series)) r <- NULL
  else {
    r <- classed(tframed(x[, series, drop = F], tframe(x)), class(x))# reconstructor
    series.names(r) <- names[series]
    }
  r
  }

# possibly there should be more attempt to preserve attributes in 
#  select.series but there are problems?:
#     at <- attributes(x)
#     atn <- names(at)
#     atl <- (atn != "dim") & (atn != "dimnames")
#     atn <- atn[atl]
#     at[[!atl]] <- NULL
#     r <- x[,series,drop=F] 
#     list.add(attributes(r), atn) <- at
     

 

tbind <- function(x, ..., pad.start=T, pad.end=T, warn=T)  {UseMethod("tbind")}

tbind.default <- function(x, ..., pad.start=T, pad.end=T, warn=T)
 {# this should work for old tsp vectors and matrices
  if (is.null(x)) stop("first argument cannot be NULL.")
  fr <- frequency(x)
  for (i in list(...)) {if (!is.null(i) && (fr != frequency(i)))
     stop("frequencies must be the same.")}
  fr <- c(fr,1)
  st <- fr %*% start(x) 
  for (i in list(...)) if (!is.null(i)) st <- min(st, fr %*% start(i) )
  en <- fr %*% end(x)
  for (i in list(...)) if (!is.null(i)) en <- max(en, fr %*% end(i) )
  r <- NULL
  # series names (sn) and names/dimnames (nm) do the same thing and sometimes
  # conflict. It is tempting to eliminate nm here, but ...
  sn <- NULL
  nm <- attr(x, "names")
  attr(x, "names") <- NULL
  for (z in append(list(x),list(...)))
   {if (!is.null(z))
    {if (is.matrix(z))
       {if (st == (fr %*% start(z))) before <- NULL
        else  before <-matrix(NA, (fr %*% start(z))-st, dim(z)[2])     
        if (en == (fr %*% end(z))) aft <- NULL
        else  aft    <-matrix(NA, en - (fr %*% end(z)), dim(z)[2])
        r <- cbind(r, rbind( before, z, aft) )
       }
     else 
       {if (st == (fr %*% start(z))) before <- NULL
        else  before <-rep(NA, (fr %*% start(z))-st)     
        if (en == (fr %*% end(z))) aft <- NULL
        else  aft <- rep(NA, en - (fr %*% end(z)))
        r <- cbind(r, c( before, z, aft) )
       }
     sn <- c(sn,series.names(z))
   }}
  if (!is.null(nm)) dimnames(r) <- list(nm,NULL)
  if (length(sn) == ncol(r)) series.names(r) <- sn
  r <- tframed(r, list(start=c((st-1)%/%fr[1], 1+(st-1)%%fr[1]), 
                       frequency=fr[1]))
  if (!(pad.start & pad.end)) r <- trim.na(r, start.=!pad.start, end.=!pad.end)
  if (is.null(r)) warning("intersection is NULL")
  r
 }

############################################################################

#   miscellaneous time calculations  <<<<<<<<<<
#   (Useful utilities not strictly part of tframe)

############################################################################

add.date <- function(date, periods, freq)
  {if (is.null(periods)) periods <- 0
   c(date[1]+(date[2]+periods-1)%/%freq, 1+(date[2]+periods-1)%%freq)
  }


###############################################

#             tests   <<<<<<<<<<<<

################################################




tframe.function.tests <- function( verbose=T, synopsis=T)
{# A short set of tests of the tframe class methods. 

  all.ok <-  T
  if (synopsis & !verbose) cat("All tframe tests ...")
  if (verbose) cat("tframe test 1 ... ")
  tspvector <- tframed(1:100, list(start=c(1981,3), frequency=4))
  data <- matrix(rnorm(300),100,3)
  tframe(data) <- tframe(tspvector)
  ok <- is.tframed(data)
  all.ok <- ok
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }


  if (verbose) cat("tframe test 2 ... ")
  ok <- test.equal(tframe(data), tframe(data))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 3 ... ")
  ok <- all(c(1981,3) == start(tspvector))
  ok <- ok & all(c(1981,3) == start(data))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 4 ... ")
  ok <- all(end(data) == end(tspvector))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 5 ... ")
  ok <- periods(data) == periods(tspvector)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 6 ... ")
  ok <- frequency(data) == frequency(tspvector)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 7 ... ")
  z <- tframed(data, list(start=c(1961,2), frequency=12) )
  ok <- is.tframed(z)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 8 ... ")
  z <- data[10:90,]
  tframe(z) <- truncate.tframe(tframe(data), start=10, end=90)
  ok <- is.tframed(z)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 9 ... ")
  z <- truncate(data, start=10, end=90)
  ok <- is.tframed(z)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 10... ")
  data <- tframed(matrix(rnorm(300),100,3), list(start=c(1961,1), frequency=12))
  z <- tfwindow(data, start=c(1963,2))
  zz <-data
  zz  <- tfwindow(zz, start=c(1963,2))
  zzz <- tfwindow(data, start=c(1963,2))
  tframe(zzz) <- tframe(z)
  zzz <- tframed(zzz, tframe(zzz))
  ok <- is.tframed(z) & is.tframed(zz) &  all(z==zz) & all(z==zzz)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 11... ")
  ok <- all( time(data) == time( tframed(data, tframe(data))))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 12... ")
  z <- tsmatrix(1:10, 11:20)
  ok <-  all(start(z) ==1) & all( z== matrix(1:20, 10,2)) 
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 13... ")
  data <- tframed(matrix(rnorm(300),100,3), list(start=c(1961,1), frequency=12))
  z <- tfwindow(data, start=c(1963,2), end=c(1969,1))
  ok <-      all(start(data)== earliest.start(data, z))
  ok <- ok & all(    end(z) == earliest.end  (data, z))
  ok <- ok & all(start(z)   == latest.start  (data, z))
  ok <- ok & all( end(data) == latest.end   (data, z))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if (verbose) cat("tframe test 14... ")
  data <- tframed(matrix(rnorm(300),100,3), list(start=c(1961,1), frequency=12))
  z <- tfwindow(data, start=c(1963,2), end=c(1969,1))
  ok <- test.equal(data, splice(z, data))
  ok <- ok & test.equal(tframe(data), tframe(splice(z, data)))
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n") else cat("failed!\n") }

  if(!exists.graphics.device()) open.graphics.device()

# dev.ask(T)
# plot(data)
 tfplot(data)

  if (synopsis) 
    {if (verbose) cat("All tframe tests completed")
     if (all.ok) cat(" OK\n") else cat(", some FAILED!\n") }
  invisible(all.ok)
}

#   2000/03/21 14:42:18 

#print.tframe.tstframe <- function(x,digits=NULL, quote = T, prefix = "", ...) 
#invisible(print(unclass(x), quote = quote))



#if( !exists("start.default"))  start.default <- start
#if( !exists("end.default")) end.default <- end
#if( !exists("frequency.default")) frequency.default <- frequency
#if( !exists("time.default")) time.default <- time


# this fix sent to r-devel  28sept98
# "tsp<-" <-function(x, value)
# {if (is.null(value)) 
#    {attr(x, "tsp") <- value
#     if(inherits(x,"ts")) class (x) <- NULL
#     return(x)
#    }
#  attr(x, "tsp") <-  value  # previously c(tf[1:2]*tf[3], tf[3])
#  class (x) <- "ts"
#  x
# }

# this fix sent to Martin 30sept98
# "tsp<-" <-function(x, value)
# {attr(x, "tsp") <-  value 
#  if (is.null(value) && inherits(x,"ts")) class(x) <- NULL
#  else class (x) <- "ts"
#  x
# }


# "tsp<-" <-function(x, value)
#   {attr(x, "tsp") <-  value
#    if (is.null(value)) 
#      {if(inherits(x,"ts")) tfclass(x) <- tfclass(x)["ts" != tfclass(x)]
#       return(x)
#      }
#    tfclass(x) <- c("ts", tfclass(x))
#    x
#   }

# This version sent by Martin  14 Oct 1998.
# Modified by PG with classed March 9, 1999. 

# in base R 0.65.0
#"tsp<-" <- function(x, value)
#{
#    cl <- class(x)
#    attr(x,"tsp") <- value
#    classed(x,
#        if (is.null(value) && inherits(x,"ts")) cl["ts" != cl] else c("ts",cl))
#}
# "tsp<-" moved to Rfixes.hs

# The R version of window.ts does not use optional
# values for start and end, but rather checks for missing arguments, so 
#  argument passing may cause problems. Also, warn is not supported and,
#  sometime in the past I found it necessary to add eps for date comparisons to
#  work properly in some situations.

#window.ts <-function(x, start=NULL, end=NULL, warn=T, eps=.Options$ts.eps)
#    {f <- tsp(x)[3]
#     if (is.null(start)) start <- tsp(x)[1]
#     if (is.null(end))   end   <- tsp(x)[2]
#     if (2 == length(start)) start <- start[1] + (start[2]-1)/f
#     if (2 == length(end))    end  <-   end[1] + (  end[2]-1)/f
#     if (start < tsp(x)[1])
#        {start <- tsp(x)[1]
#         if (warn) warning("Specified start earlier than start of data, start not changed.")
#        }
#     if (end > tsp(x)[2])
 #       {end <- tsp(x)[2]
#         if (warn) warning("Specified end later than end of data, end not changed.")
#        }
#     leave <- (time(x) >= (start-eps)) & (time(x) <= (end+eps))
#     #Rbug unclass shouldn't be needed in the next line
#     if (is.matrix(x)) z <- unclass(x)[leave,,drop=F] 
#     else  z <- x[leave]
##     tsp(z) <- c(start, end, f)
#     classed(ts(z, start=start, end=end, frequency=f), tfclass(x))
#    }


# Martin M's version in R 62.3 seems better.
# matplot <- function(x, y, type ="p", lty=1:3, xlab="x", ylab="y", 
#               colors=c("black", "blue", "red", "green", "cyan"), ...) 
#   {# lty only affects type="l"
#    # if lty is not long enough it is repeated using colors so set lty=1 to
#    #  make each plot a different colour.
#    if (!is.matrix(y)) y <- matrix(y, length(y),1)
#    # vector or column matrix is repeated for each column of y:
#    if ((is.matrix(x)) && (ncol(x)==1)) x <- c(x) 
#    if (!is.matrix(x)) x <- matrix(x, length(x), dim(y)[2])
#    if (!all(dim(x) == dim(y)))
#        stop("matplot array dimensions do not correspond.")
#    colors <- c(t(matrix(colors, length(colors), length(lty))))
#    if (length(colors) < ncol(y)) colors <-(rep(colors, ncol(y)))[seq(ncol(y))]
#    if (length(lty) < ncol(y)) lty <- (rep(lty, ncol(y)))[seq(ncol(y))]
#    for (i in 1:ncol(x)) 
#      {if (i ==1) tfplot.default(x[,i],y[,i],xlim=range(x[!is.na(x)]), 
#         ylim=range(y[!is.na(y)]),
#         xlab=xlab, ylab=ylab, type=type, lty=lty[i], col=colors[i], ...)
#       else lines(x[,i],y[,i], type=type, lty=lty[i], col=colors[i],  ...)
#      }
#    invisible()
#   }



###############################################

#  tstframe (ts) specific methods   <<<<<<<<<<<<

################################################

tframe.tstframe <-function(x)
 {classed(tsp(x), c("tstframe", "tframe"))}  # constructor

start.tframe.tstframe <-function(tf) {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tstframe <-function(tf) {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tstframe <-function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tstframe <-function(tf) {tf[3]}

time.tframe.tstframe <-function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

truncate.tframe.tstframe <-function(tf, start=NULL, end=NULL) 
    {if (!is.null(end))   tf[2] <- tf[1] + (end-1)/tf[3]
     if (!is.null(start)) tf[1] <- tf[1] + (start-1)/tf[3]
     tf
    }

expand.tframe.tstframe <-function(tf, add.start=0, add.end=0) 
    {tf[2] <- tf[2] + add.end/tf[3]
     tf[1] <- tf[1] - add.start/tf[3]
     tf
    }


earliest.start.index.tframe.tstframe <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] < args[[r]][1]) r <- i
         }           
     r
    }

earliest.end.index.tframe.tstframe <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] < args[[r]][2]) r <- i
         }           
     r
    }

latest.start.index.tframe.tstframe <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[1] > args[[r]][1]) r <- i
         }           
     r
    }

latest.end.index.tframe.tstframe <-function(x, ...) 
    {r <- 1
     fr <- frequency(x)
     args <- list(x, ...)
     for (i in seq(length(args)))
         {tf <- args[[i]]
          if (tf[3] != fr) stop("frequencies must be that same.")
          if (tf[2] > args[[r]][2]) r <- i
         }           
     r
    }
#   2000/04/20 14:49:44  




##############################################################################

#  The first section of this file contains generic definitions for objects 
#   which describe a database source for time series data. 

#  Following that are methods for the object tfPADIdata.

############################################################################

# Note: the constructors (e.g. tfPADIdata, TSPADIdata) cannot be generic.



modify <- function (x, ...) {UseMethod("modify")} 


freeze <- function (data, ...) {
#This function allows for the possiblity of data structures which invoke a
#    call to a database.  It is called by functions which actually use data
# eg:    data <- freeze(data)
# in order to take a snapshot from the database and bring it into a 
# structure which can be used for calculations.
    UseMethod("freeze")
} 
 
freeze.default  <- function(data)  
  {if ("character"==mode(data)) freeze(tfPADIdata(data, server="ets")) else data} 


availability <- function (data, ...) UseMethod("availability")


availability.default<-function(data.id, names=NULL, server="ets", dbname="",
                       verbose=T, timeout=60, stop.on.error=TRUE, warn=TRUE)  
{# Indicate  dates for which data is available. 
 # data.id should be a character vector of data identifiers  
 if (1== length(server)) server  <- rep(server, length(data.id))
 if (1== length(dbname)) dbname  <- rep(dbname, length(data.id))

 # next 3 lines are to look after older style name forms at the BOC
 ets <- "ets" == substring(dbname,1,3)
 server[ets] <-"ets"
 dbname[ets] <- ""
 
 # server[server=="ets"] <- "padi"   # temporary kludge at the BOC

 s <- e <- f <- NULL
 for (i in 1:length(data.id))
      {data <- getpadi(data.id[i], dbname=dbname[i], server=server[i],
                stop.on.error=stop.on.error, use.tframe=T, warn=warn, 
                pad=F, timeout=timeout)
       s <- rbind(s, start(data))
       e <- rbind(e, end(data))
       f <- c(f,frequency(data))
       if (verbose)
         {cat(data.id[i]," from: ",start(data))
          cat("  to: ",end(data))
          cat("   frequency ", frequency(data))
          if (!is.null(names)) cat("  ",names[i])
          cat("\n")
      }  }
  invisible(list(start=s, end=e, frequency=f, series=data.id))
}



refresh <- function(data)
{src <- source.info(data)
 if (is.null(src)) stop("data must include source information to use refresh.")
 freeze(src)
}


# extract series source.info  (this is used by refresh so the result should
#   be the correct class, etc.
source.info <- function(obj)UseMethod("source.info")

source.info.default <- function(obj){
    if (is.null(attr(obj, "source"))) stop("object does not have source information.")
    attr(obj, "source")
   }

# extract series identifiers
identifiers <- function(obj)UseMethod("identifiers")

identifiers.default <- function(obj){
    if (is.null(attr(obj, "source"))) stop("object does not have source information.")
    attr(obj, "source")[1,]
   } 
   

# extract series sourcedb
sourcedb <- function(obj)UseMethod("sourcedb")

sourcedb.default <- function(obj){
    if (is.null(attr(obj, "source"))) stop("object does not have source information.")
    attr(obj, "source")[3,]
   } 
   

# extract series sourceserver
sourceserver <- function(obj)UseMethod("sourceserver")

sourceserver.default <- function(obj){
    if (is.null(attr(obj, "source"))) stop("object does not have source information.")
    attr(obj, "source")[2,]
   }
   
############################################################################

#    functions define a time series matrix class "tfPADIdata"    <<<<<<<<<<
#      which uses the TSPADI  data interface. See also dsepadi   <<<<<<<<<<
#      file uses the methods here to define TSdata.              <<<<<<<<<<

############################################################################

# The PADI interface uses some calls to operating system specific functions:
#    -the function sleep is used in TSPADI.function.tests  
#    -the function local.host.netname defined in syskern.s
#    -the function user.name defined in the PADI interface software calls a
#        program (getpwuid) in the $PADI/bin. This was previously done with
#        whoami() in syskern.s, which uses /usr/ucb/whoami (not system V unix).
#        It is important that user.name() return the same result as the C
#        function getpwuid in order for the padi interface to work properly.


############################################################

#   Definition of class c("tfPADIdata") <<<<<<<<<<

############################################################



tfPADIdata <- function( series,  server = "", db= "", transforms= "",  
           start=NA, end=NA, frequency=NA, names=NULL, 
           pad=FALSE, pad.start=pad, pad.end=pad,
           use.tframe=T,
           start.server=NULL, server.process=NULL, cleanup.script=NULL,
           stop.on.error=T, warn=T)
  {# This is the constructor (but see set.tfPADIdata for a prompter).
   if (is.null(series)) return(NULL)
   if (is.null(names))   names <- series
   if(length(series) != length(names) )
           stop("number of names does not match number of series.")
   r <- rbind(series, server, db, transforms)
   dimnames(r) <- list(c("series", "server", "db", "transforms") ,names)
   attr(r, "start")     <- start
   attr(r, "end")       <- end
   attr(r, "frequency") <- frequency
   attr(r, "pad.start") <- pad.start
   attr(r, "pad.end")   <- pad.end
   attr(r,"use.tframe") <- use.tframe 

   attr(r, "start.server") <- start.server
   attr(r, "server.process") <- server.process
   attr(r, "cleanup.script") <- cleanup.script
   attr(r, "stop.on.error") <- stop.on.error
   attr(r, "warn")      <- warn
   class(r) <- "tfPADIdata"
   r
   }



set.tfPADIdata <- function(preamble=T)
 {# prompt for series identifiers, set class, etc.
  if (preamble) 
    {cat("This function prompts for the names and database locations for\n")
     cat("series, until an empty line is entered.\n\n")
     cat("Variables...\n")
    }
  series <- server <- db <- transforms <- NULL
  repeat
    {key <- readline("  series..")
# cat(":",key,":") there seems to be a bug here. readline is not flushing
     if (""== key) break  else series <-c(series, key)
     server     <- c(server,     readline("  server.."))
     db         <- c(db,         readline("  database.."))
     transforms <-c(transforms,  readline("  transformation.."))
    } 
  if (is.null(series)) return(NULL) 
  cat("  starting year..");key <- readline()
     if (!(""== key)) 
       {start. <- as.integer(key)
        cat("  starting period..");key <- readline()
        start. <- c(start., as.integer(key))
        if(any(is.na(start.)))
            cat("Warning: start improperly specified. NOT set!")
        }
     else start. <- NA
  cat("  ending year..");key <- readline()
     if (!(""== key)) 
       {end. <- as.integer(key)
        cat("  ending period..");key <- readline()
        end. <- c(end., as.integer(key))
        if(any(is.na(end.))) cat("Warning: end improperly specified. NOT set!")
        }
     else end. <- NA

  data <- tfPADIdata(series, server=server, db=db, transforms=transforms,
                     start=start., end=end.)
  if (preamble) 
    {cat("The series may now be retrieved, in which case the data is\n")
     cat("  fixed as currently available, or they may be left `dynamic',\n")
     cat("  in which case they are retrieved using freeze.\n")
     cat("Retrieve data y/n:");key <- readline()
     if ((key =="y") | (key=="Y")) data <- freeze(data)
    }
  data
}


modify.tfPADIdata <- function( r, append=NA, 
           series=NA, server=NA, db=NA, transforms=NA, 
           start=NA, end=NA, frequency=NA, names=NA, 
           pad=NA, pad.start=NA, pad.end=NA,
           use.tframe=NA,
           start.server=NA, server.process=NA, cleanup.script=NA,
           stop.on.error=NA, warn=NA)
  {
   if (!is.na(series))     r[1,] <- series
   if (!is.na(server))     r[2,] <- server
   if (!is.na(db))         r[3,] <- db
   if (!is.na(transforms)) r[4,] <- transforms
   
   if (!is.na(names))
       dimnames(r) <- list(c("series", "server", "db", "transforms") ,names)

  if (!all(is.na(append))) 
    {if (is.null(append$series))     append$db <- ""
     if (is.null(append$db))         append$db <- ""
     if (is.null(append$transforms)) append$transforms <- ""
     if (is.null(append$names))      append$names <- append$series
     if(length(append$series) != length(append$names) )
           stop("number of new names does not match number of new series.")
     newr <- rbind(append$series, append$server, append$db, append$transforms)
     newr <- cbind(r,newr)
     dimnames(newr) <- list(c("series", "server", "db", "transforms") ,
                            c(dimnames(r)[[2]], append$names))
     attr(newr, "start")     <- attr(r, "start") 
     attr(newr, "end")       <- attr(r, "end")
     attr(newr, "frequency") <- attr(r, "frequency")
     attr(newr, "pad.start") <- attr(r, "pad.start")
     attr(newr, "pad.end")   <- attr(r, "pad.end")
     attr(newr,"use.tframe") <- attr(r,"use.tframe")  

     attr(newr, "start.server")   <- attr(r, "start.server")
     attr(newr, "server.process") <- attr(r, "server.process")
     attr(newr, "cleanup.script") <- attr(r, "cleanup.script")
     attr(newr, "stop.on.error")  <- attr(r, "stop.on.error") 
     attr(newr, "warn")           <- attr(r, "warn")  
     class(newr) <- "tfPADIdata"
     r <- newr
    }
   
   if (!any(is.na(start)))          attr(r, "start")     <- start
   if (!any(is.na(end)) )           attr(r, "end")       <- end
   if (!    is.na(frequency))       attr(r, "frequency") <- frequency

   if (!is.na(pad))            pad.start<- pad.end<- pad
   if (!is.na(pad.start))      attr(r, "pad.start") <- pad.start
   if (!is.na(pad.end))        attr(r, "pad.end")   <- pad.end
   if (!is.na(use.tframe))     attr(r,"use.tframe") <- use.tframe 

   if (!is.na(start.server))   attr(r, "start.server") <- start.server
   if (!is.na(server.process)) attr(r, "server.process") <- server.process
   if (!is.na(cleanup.script)) attr(r, "cleanup.script") <- cleanup.script
   if (!is.na(stop.on.error))  attr(r, "stop.on.error") <- stop.on.error
   if (!is.na(warn))           attr(r, "warn")      <- warn
   r
   }




############################################################

#     methods for tfPADIdata class objects <<<<<<<<<<

# See also freeze.tfPADIdata and availability.tfPADIdata further below

############################################################


is.tfPADIdata <-function(obj) {inherits(obj, "tfPADIdata") }

print.tfPADIdata <- function(x, ...)
  {print.default(x)
   invisible(x)
  }

start.tfPADIdata <- function(x)
     {if(is.null(attr(x, "start"))) NA else attr(x, "start")}
end.tfPADIdata   <- function(x)
     {if(is.null(attr(x, "end")))   NA else attr(x, "end")}
frequency.tfPADIdata <- function(x)
     {if(is.null(attr(x, "frequency")))   NA else attr(x, "frequency")}
periods.tfPADIdata <- function(data) NA  # could be better
series.names.tfPADIdata <- function(data) {dimnames(data)[[2]]}
# nseries default should work


identifiers.tfPADIdata  <- function(obj)  {obj[1,]}
sourceserver.tfPADIdata <- function(obj)  {obj[2,]}
sourcedb.tfPADIdata     <- function(obj)  {obj[3,]}
source.info.tfPADIdata  <- function(obj)  {attr(obj,"source")} #used by refresh


"[.tfPADIdata" <- function (x, i, j, drop = FALSE) #N.B. FALSE
   {a <- attributes(x)
    y <- NextMethod("[")
    a$dim      <- dim(y)
    a$dimnames <- dimnames(y)
    attributes(y) <- a
    y
   }

tsp.tfPADIdata <-function(x)
  {start. <-start(x)
   end.   <-  end(x)
   f <- frequency(x)
   if (length(start.)==2) start. <- start.[1] + (start.[2]-1)/f
   if (length(end.)==2)   end.   <- end.[1]   + (end.[2]-1)/f
   c(start., end., f)
  }

 

############################################################

#      Database interface for tfPADIdata  <<<<<<<<<<

############################################################



freeze.tfPADIdata <- function(data, timeout=60)
{ # This function retreives data from a PADI server using getpadi
  # A server specified as NULL or as "" is expanded to the localhost.

   # next 3 lines are to look after older style name forms at the BOC
   ets <- "ets" == substring(data["db",],1,3)
   data["server", ets] <- "ets"
   data["db",     ets] <- ""

   data["server", data["server",] ==""] <- local.host.netname() 

   # missing attr is NULL but should be translated to getpadi defaults:
   IfNull <- function(a,b) {c(a,b)[1]}

   r  <- getpadi( data["series",], server=data["server",], dbname=data["db",],
     start.server=   IfNull(attr(data,"start.server"), T),
     server.process= IfNull(attr(data,"server.process"), padi.server.process()),
     cleanup.script= IfNull(attr(data,"cleanup.script"), padi.cleanup.script()),
     starty= if(any(is.na(start(data)))) 0 else start(data)[1],
     startm= if(any(is.na(start(data)))) 0 else start(data)[2],
     endy=   if(any(is.na(end(data))))   0 else end(data)[1],
     endm=   if(any(is.na(end(data))))   0 else end(data)[2],
     transformations = data["transforms",],
     pad  = (attr(data,"pad.start") | attr(data,"pad.end") ),
     user =          IfNull(attr(data,"user"), user.name() ),
     passwd=         IfNull(attr(data,"passwd"),       ""  ),
     stop.on.error = IfNull(attr(data,"stop.on.error"), T  ),
     use.tframe=     IfNull(attr(data,"use.tframe"),    F  ), 
     warn=           IfNull(attr(data,"warn"),          T  ),
     timeout= timeout)

 if (is.character(r)) stop(r)
 if (!attr(data,"pad.start")) r <- trim.na(r, start.=T, end.=F)
 if (!attr(data,"pad.end") )  r <- trim.na(r, start.=F, end.=T)
 if (dim(r)[2] != dim(data)[2]) stop("Error retrieving data.")
 if ( !is.na(frequency(data)) && (frequency(data)) != frequency(r))
       warning("returned data frequency differs from request.")
 series.names(r) <- series.names(data)
 attr(r, "source") <- data 
 attr(r, "retrieval.date") <- date.parsed() 
 r
}

availability.tfPADIdata<-function(data, verbose=T, timeout=60)  
{# Indicate  dates for which data is available.
 # This requires retrieving series individually so they are not truncated.

   # next 3 lines are to look after older style name forms at the BOC
   ets <- "ets" == substring(data["db",],1,3)
   data["server", ets] <- "ets"
   data["db",     ets] <- ""

   data["server", data["server",] ==""] <- local.host.netname() 
   series <- data["series", ]
   s <- e <- f <- NULL
   for (i in 1:length(series))
     {r <- getpadi( series[i], server=data["server",i], dbname=data["db",i], 
        start.server   = attr(data,"start.server"), 
        server.process = attr(data,"server.process"),
        cleanup.script = attr(data,"cleanup.script"),
        starty=if(any(is.na(start(data)))) 0 else start(data)[1],
        startm=if(any(is.na(start(data)))) 0 else start(data)[2],
        endy=if(any(is.na(end(data))))  0 else end(data)[1],
        endm=if(any(is.na(end(data))))  0 else end(data)[2],
        transformations = data["transforms",i],
        pad  = (attr(data,"pad.start") | attr(data,"pad.end")) ,
        user =if(is.null(attr(data,"user"))) user.name() else attr(data,"user"),
        passwd=if(is.null(attr(data,"passwd")))  ""    else attr(data,"passwd"),
        stop.on.error = attr(data,"stop.on.error"),
        use.tframe=attr(data,"use.tframe"), 
        warn=attr(data,"warn"), timeout=timeout)

       s <- rbind(s, start(r))
       e <- rbind(e, end(r))
       f <- c(f,frequency(r))
       if (verbose)
         {cat(series[i]," from: ",start(r))
          cat("  to: ",end(r))
          cat("   frequency ", frequency(r))
          cat("  ",series.names(data)[i])
          cat("\n")
      }  }
  invisible(list(start=s, end=e, frequency=f, series=series))
}



tfputpadi  <- function (data,  
         server = local.host.netname(),
         dbname = "", 
         series = series.names(data),
         start.server = T,
         server.process = padi.server.process(), 
         cleanup.script = padi.cleanup.script(),
         user = user.name(), passwd= "",
         stop.on.error = T, warn = T)   
  {# This is just putpadi with a tfPADIdata object returned suitable for 
   #   retrieving the data.

   ok <- putpadi(data, server=server, dbname=dbname, series=series,
         start.server = start.server, server.process=server.process, 
         cleanup.script=cleanup.script,
         user=user, passwd=passwd,
         stop.on.error=stop.on.error, warn=warn ) 

   if (!all(ok)) stop("error putting data on database.")
  
   tfPADIdata( series, server=server, db=dbname, transforms="",  
           start=start(data), end=end(data), frequency=frequency(data), 
           names=series, pad=FALSE, 
           use.tframe=T, stop.on.error=stop.on.error, warn=warn)
  }


#   The following function is supplied separately (with PADI ). The 
#   documentation is included here so it will integrate with DSE.

#######################################################################

#     functions for converting defunct format FAMEdata structure
#         (these are primarily for use at the BOC)

#######################################################################

freeze.FAMEdata <-function(data)
  {stop("FAMEdata is defunct. Use FAMEdata.to.tfPADIdata to convert the structure")}


#######################################################################


#######################################################################

#    tfPADI interface tests (from Brief User's Guide)   <<<<<<<<<<

#######################################################################


tfPADI.function.tests <- function( verbose=T, synopsis=T,
      fuzz.small=1e-14, fuzz.large=1e-6)
{# test for TSPADI access using simple.server

 # These tests only check that the tfPADI structures work. For a more
 #   complete set of PADI tests see the file padi.s distributed 
 #   with the TS PADI software.


  if (synopsis & !verbose) cat("tfPADI tests ...")

  scratch.db <-"zot123456.db"
  unlink(scratch.db)
  server <- local.host.netname()

 if (verbose) cat("tfPADI test 0 ... ")
  if (check.padi.server(server))
     stop("A server is already running. Testing stopped. Use cleanup.padi.server() or kill.padi.server() to terminate it.")

  pid <- start.padi.server(server=server, dbname="", 
                 server.process=paste("simple.server ", scratch.db))
  on.exit(cleanup.padi.server(pid, cleanup.script="cleanup.simple.server"))

  # wait to ensure padi server is started
     for (i in 1:30)
       {if (check.padi.server(server)) break
        sleep(1)
       }
  all.ok <- ok <- T
  if (verbose) 
    {if (ok) cat("ok\n")
     else  cat("failed! starting server\n")
    }


  if (verbose) cat("tfPADI test 1 ... ")

  eg.put.data <- tframed(matrix(c(1*exp(1:20),2*exp(1:20)),20,2), 
                         list(start=c(1950,1),freq=1))
  series.names(eg.put.data) <- c("exp1", "exp2")

  if (any(series.names(eg.put.data) != c("exp1", "exp2")))
    stop("series.name setting is not working properly. Other tests will fail.")

  eg.names <- tfputpadi(eg.put.data,
                      dbname=scratch.db, server=server,
                      start.server=T, server.process="simple.server", 
                      cleanup.script="cleanup.simple.server",
                      stop.on.error=T, warn=T )
  ok<-is.tfPADIdata(eg.names) 
  all.ok <- ok
  if (verbose) 
    {if (ok) cat("ok\n")
     else  cat("failed! tfputpadi\n")
    }

  if (verbose) cat("tfPADI test 2 ... ")
  eg.data <- freeze(eg.names)
  ok <- is.tfPADIdata(eg.names) &
            test.equal(eg.data, eg.put.data, fuzz=fuzz.large)
  all.ok <- all.ok & ok 
  if (verbose) {if (ok) cat("ok\n")  else cat("failed!\n") }


  on.exit()
  cleanup.padi.server(pid, cleanup.script="cleanup.simple.server")

  if (synopsis) 
    {if (verbose) cat("All tfPADI tests completed")
     if (all.ok) cat(" OK\n") else cat(", some FAILED!\n")
    }

  invisible(all.ok)
}
