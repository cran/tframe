
###########################################################################

#  Internal utilities to limit dependence on class/oldclass

if (is.R())
  { tfclass    <- .Alias(class)
   "tfclass<-" <- .Alias(get("class<-"))
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
is.tframe <- function(tf) inherits(tf, "tframe")
is.tframed <- function (x) inherits(tframe(x), "tframe")

tframe <- function(x) {UseMethod("tframe") } #extract the tframe

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

"tframe<-" <- function(x, value)
 {if(is.null(value)) UseMethod("tframe<-")
  else settf(value,x)
 }

"tframe<-.default" <- function(x, value) {tsp(x) <- value; x}

settf <- function(value, x) {UseMethod("settf") }

settf.default <- function(value, x)
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


tfprint.tframe <- function(x, digits=NULL, quote=T, prefix="", ...) 
   UseMethod("tfprint.tframe")

tfprint.tframe.default <- function(x, digits=NULL, quote=T, prefix="", ...) 
   invisible(print(unclass(x), quote=quote, prefix=prefix, ...))
   # digits=digits, seems to cause problems ?



start.tframe <- function(tf)UseMethod("start.tframe")
end.tframe <- function(tf)UseMethod("end.tframe")

# periods should give the number of data points in the time direction.
periods.tframe <- function(tf)UseMethod("periods.tframe")

# frequency is less essential and may not always make sense.
frequency.tframe <- function(tf)UseMethod("frequency.tframe")

time.tframe <- function(tf)UseMethod("time.tframe")

truncate.tframe <- function(tf, start=NULL, end=NULL)
    {#NULL means no truncation.
     UseMethod("truncate.tframe")
    }

expand.tframe <- function(tf, add.start=0, add.end=0)
     UseMethod("expand.tframe")



is.consistent.tframe <- function(tf, x) UseMethod("is.consistent.tframe")

is.consistent.tframe.default <- function(tf, x)
   {periods.tframe(tf) == periods(x)}

test.equal.tframe <- function(tf1, tf2) UseMethod("test.equal.tframe")

test.equal.tframe.default <- function(tf1, tf2) { all(tf1==tf2)}

# Following could be used to do date comparisons like start() < end()

earliest.start.index.tframe <- function(x, ...)
    UseMethod("earliest.start.index.tframe")

earliest.start.tframe <- function(x, ...)
    append(list(x),list(...))[[earliest.start.index.tframe(x, ...)]]

earliest.end.index.tframe <- function(x, ...)
    UseMethod("earliest.end.index.tframe")

earliest.end.tframe <- function(x, ...)
    append(list(x),list(...))[[earliest.end.index.tframe(x, ...)]]

latest.start.index.tframe <- function(x, ...)
    UseMethod("latest.start.index.tframe")

latest.start.tframe <- function(x, ...)
    append(list(x),list(...))[[latest.start.index.tframe(x, ...)]]

latest.end.index.tframe <- function(x, ...)
    UseMethod("latest.end.index.tframe")

latest.end.tframe <- function(x, ...)
    append(list(x),list(...))[[latest.end.index.tframe(x, ...)]]


###############################################

#  default .tframe methods   <<<<<<<<<<<<

################################################

start.tframe.default <- function(tf) {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.default <- function(tf) {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.default <- function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.default <- function(tf) {tf[3]}

time.tframe.default <- function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

truncate.tframe.default <- function(tf, start=NULL, end=NULL) 
    {# like window but uses indexes rather than dates
     if (!is.null(end))   tf[2] <- tf[1] + (end-1)/tf[3]
     if (!is.null(start)) tf[1] <- tf[1] + (start-1)/tf[3]
     tf
    }

expand.tframe.default <- function(tf, add.start=0, add.end=0) 
    {tf[2] <- tf[2] + add.end/tf[3]
     tf[1] <- tf[1] - add.start/tf[3]
     tf
    }


earliest.start.index.tframe.default <- function(x, ...) 
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

earliest.end.index.tframe.default <- function(x, ...) 
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

latest.start.index.tframe.default <- function(x, ...) 
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

latest.end.index.tframe.default <- function(x, ...) 
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

#"tframe.tframe<-.ts"  <- function(x, value) {tsp(x) <- value; x}

tframe.ts <- function(x){classed(tsp(x), c("tstframe", "tframe"))} # constructor

"tframe<-.ts"      <- function(x, value) {tsp(x) <- value; x}

#settf.default works for .ts

start.tframe.tstframe <- function(tf)
   {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tstframe <- function(tf)
   {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tstframe <- function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tstframe <- function(tf) {tf[3]}

time.tframe.tstframe <- function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}



###############################################

#   .tf and .tframe.tftframe methods

# these provide .ts or tsp like methods for more general arrays with time as the
#   first dimension. It may be possible extend to more general structures.

###############################################

tframe.tf <- function(x) {attr(x, "tf") }

"tframe<-.tf" <- function(x, value)
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


settf.tftframe <- function(value, x)
{#class(value) <- c("tftframe", "tframe")
 if (!is.consistent.tframe(value, x))
    stop("time frame value in tframe assignment is not consistent with data.")
 attr(x, "tf") <- value
 classed(x, "tf")  # constructor (settf.tftframe)
}


start.tf <- function(x) {start(tframe(x))}
end.tf   <- function(x) {end(tframe(x))}
periods.tf <- function(x) {periods(tframe(x))}
frequency.tf <- function(x) {frequency(tframe(x))}
time.tf  <- function(x) {time(tframe(x))}

start.tframe.tftframe <- function(tf)
   {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tftframe <- function(tf)
   {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tftframe <- function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tftframe <- function(tf) {tf[3]}

time.tframe.tftframe <- function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

tfwindow.tf <- function  (x, start=NULL, end=NULL, warn=T, eps=.Options$ts.eps) 
  {# this needs work
   tfwindow(ts(x, start=start(x), frequency=frequency(x)),
             start=start, end=end, warn=warn)
  }

###############################################

#   .TSdata methods (for DSE)

###############################################


"tframe<-.TSdata" <- function(x, value)
 {if (0 != input.dimension(x)) tframe( input.data(x)) <- value
  if (0 !=output.dimension(x)) tframe(output.data(x)) <- value
  x
 }



###############################################

#   .rts and .tframe.rtstframe methods

###############################################



"tframe<-.rts"     <- function(x, value) {rts(x) <- value; x}
"tframe.tframe<-.rts" <- function(value, x)
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
is.consistent.tframe.stamped <- function(tf, x)
  {periods(x) == periods(tf)}

test.equal.tframe.stamped <- function(tf1, tf2)
  {all(tf1$stamp == tf2$stamp)}

periods.tframe.stamped <- function(x)length(tframe(x))



###############################################



test.equal <- function(obj1, obj2, ...) UseMethod("test.equal")

 
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

lag.tframe <- function(x,...)UseMethod("lag.tframe") 

if (!exists("lag"))  lag.default <- function(x, ...) {stop("no lag function") }




splice <- function(obj1, obj2, ...) UseMethod("splice")

splice.default <- function(mat1, mat2)
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

truncate.default <- function(x, start=NULL, end=NULL)
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

expand.default <- function(x, start=NULL, end=NULL)
    {tf <- expand.tframe(tframe(x), start, end)
     select.series(tbind(x,time(tf)), series=1)
    }


earliest.start <- function(x, ...)
    start(append(list(x),list(...))[[earliest.start.index(x, ...)]])

earliest.start.index <- function(x, ...)
  {if (is.tframe(x)) UseMethod("earliest.start.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("earliest.start.index.tframe", tf)
     }
   r
  }

earliest.end <- function(x, ...)
    end(append(list(x),list(...))[[earliest.end.index(x, ...)]])

earliest.end.index <- function(x, ...)
  {if (is.tframe(x)) UseMethod("earliest.end.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("earliest.end.index.tframe", tf)
     }
   r
  }

latest.start <- function(x, ...)
    start(append(list(x),list(...))[[latest.start.index(x, ...)]])

latest.start.index <- function(x, ...)
  {if (is.tframe(x)) UseMethod("latest.start.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("latest.start.index.tframe", tf)
     }
   r
  }

latest.end <- function(x, ...)
    end(append(list(x),list(...))[[latest.end.index(x, ...)]])

latest.end.index <- function(x, ...)
  {if (is.tframe(x)) UseMethod("latest.end.index.tframe")
   else 
     {tf <- list(tframe(x))
      for (i in list(...)) tf <- append(tf, list(tframe(i)))
      r <- do.call("latest.end.index.tframe", tf)
     }
   r
  }



trim.na <- function(obj, ...) UseMethod("trim.na") 

trim.na.default <- function(x, start.=T, end.=T)
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
nseries.default <- function(x)  {ncol(x)} 

   

 series.names <- function(x)       UseMethod("series.names")
"series.names<-" <- function(x, value)UseMethod("series.names<-")

 series.names.default <- function(x)
   {if (is.null(x)) return(NULL)
    names <- attr(x, "series.names")
    if (is.null(names)) names <- dimnames(x)[[2]]
    if (is.null(names)) names <- paste("Series", seq(ncol(x)))
    names
   }

"series.names<-.default" <- function(x, value){attr(x,"series.names")<-value; x}



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

