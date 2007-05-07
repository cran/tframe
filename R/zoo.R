
tframe.zoo <- function (x) {
  tf <- index(x)
  class(tf) <- c( "zootframe", class(tf), "tframe")
  tf
  }

tfSet.zootframe <- function(value, x) {
   r <- zoo(x, value) 
   if (inherits(r, "try-error")) {r <- x ; attr(r, "tframe") <- value}
   r
   }

tfstart.zoo    <- function(x) start(x)
tfend.zoo      <- function(x) end(x)
tfperiods.zoo <- function(x) if(is.matrix(x)) nrow(x) else length(x)

tfstart.zootframe <- function(x) x[1]
tfend.zootframe   <- function(x) x[length(x)]
tfperiods.zootframe   <- function(x) length(x)
periods.zootframe     <- function(x) length(x)

tfwindow.zoo <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  {# With the default warn=T warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   y <- window(x, start=start, end=end)
   seriesNames(y) <- seriesNames(x)
   y
  }

tbind.zoo <- function(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
 {nm <- seriesNames(x)
  for (z in list(...)) {
    if (!is.null(z)) {
      nm <- c(nm, seriesNames(z))
      x <- cbind(x, z)
      }
    }
  if (!pad.start | !pad.end)
     x <- trimNA(x, startNAs= !pad.start, endNAs= !pad.end)
  seriesNames(x) <- nm
  x
 }  

