
tframe.its <- function (x) {
  tf <- dates(x)
  class(tf) <- c( "itstframe", class(tf), "tframe")
  tf
  }

tfSet.itstframe <- function(value, x) {
   r <- its(x, value) 
   if (inherits(r, "try-error")) {r <- x ; attr(r, "tframe") <- value}
   r
   }

tfstart.its <- function(x) dates(x)[1] # start(x) returns character rather than a date
tfend.its   <- function(x) dates(x)[periods(x)]
time.its    <- function(x, ...) dates(x, ...)

tfstart.itstframe <- function(x) x[1]
tfend.itstframe   <- function(x) x[length(x)]
tfperiods.itstframe   <- function(x) length(x)
periods.itstframe     <- function(x) length(x)

tfwindow.its <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  {# With the default warn=T warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   d <- dates(x)
   i <- rep(TRUE, periods(x))
   if(!is.null(start)) i <- i &  
             (d >= if(is.character(start)) as.POSIXct(start) else start)
   if(!is.null(end))   i <- i &  
             (d <= if(is.character(end)) as.POSIXct(end) else end)
   y <- its(x[i,, drop=FALSE], d[i])
   seriesNames(y) <- seriesNames(x)
   y
  }

tbind.its <- function(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
 {nm <- seriesNames(x)
  for (z in list(...)) {
    if (!is.null(z)) {
      nm <- c(nm, seriesNames(z))
      x <- union(x, z)
      }
    }
  if (!pad.start | !pad.end)
     x <- trimNA(x, startNAs= !pad.start, endNAs= !pad.end)
  seriesNames(x) <- nm
  x
 }  

