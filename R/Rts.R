
#print.tframe.tstframe <- function(x,digits=NULL, quote = T, prefix = "", ...) 
#invisible(print(unclass(x), quote = quote))



#if( !exists("start.default"))  start.default <- start
#if( !exists("end.default")) end.default <- end
#if( !exists("frequency.default")) frequency.default <- frequency
#if( !exists("time.default")) time.default <- time


# this fix sent to r-devel  28sept98
# "tsp<-" <- function(x, value)
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
# "tsp<-" <- function(x, value)
# {attr(x, "tsp") <-  value 
#  if (is.null(value) && inherits(x,"ts")) class(x) <- NULL
#  else class (x) <- "ts"
#  x
# }


# "tsp<-" <- function(x, value)
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

#window.ts <- function(x, start=NULL, end=NULL, warn=T, eps=.Options$ts.eps)
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

tframe.tstframe <- function(x)
 {classed(tsp(x), c("tstframe", "tframe"))}  # constructor

start.tframe.tstframe <- function(tf) {c(floor(tf[1]), round(1 +(tf[1]%%1)*tf[3]))}

end.tframe.tstframe <- function(tf) {c(floor(tf[2]), round(1 + (tf[2]%%1)*tf[3]))}

periods.tframe.tstframe <- function(tf)  {1+round((tf[2]-tf[1])*tf[3])}

frequency.tframe.tstframe <- function(tf) {tf[3]}

time.tframe.tstframe <- function(tf) {tf[1] + (seq(periods(tf))-1)/tf[3]}

truncate.tframe.tstframe <- function(tf, start=NULL, end=NULL) 
    {if (!is.null(end))   tf[2] <- tf[1] + (end-1)/tf[3]
     if (!is.null(start)) tf[1] <- tf[1] + (start-1)/tf[3]
     tf
    }

expand.tframe.tstframe <- function(tf, add.start=0, add.end=0) 
    {tf[2] <- tf[2] + add.end/tf[3]
     tf[1] <- tf[1] - add.start/tf[3]
     tf
    }


earliest.start.index.tframe.tstframe <- function(x, ...) 
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

earliest.end.index.tframe.tstframe <- function(x, ...) 
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

latest.start.index.tframe.tstframe <- function(x, ...) 
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

latest.end.index.tframe.tstframe <- function(x, ...) 
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
