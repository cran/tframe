
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
 
freeze.default <- function(data)  
  {if ("character"==mode(data)) freeze(tfPADIdata(data, server="ets")) else data} 


availability <- function (data, ...) UseMethod("availability")


availability.default <- function(data.id, names=NULL, server="ets", dbname="",
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


is.tfPADIdata <- function(obj) {inherits(obj, "tfPADIdata") }

print.tfPADIdata <- function(x, ...)
  {print.default(x)
   invisible(x)
  }

start.tfPADIdata <- function(x)
     {if(is.null(attr(x, "start"))) NA else attr(x, "start")}
end.tfPADIdata <- function(x)
     {if(is.null(attr(x, "end")))   NA else attr(x, "end")}
frequency.tfPADIdata <- function(x)
     {if(is.null(attr(x, "frequency")))   NA else attr(x, "frequency")}
periods.tfPADIdata <- function(data) NA  # could be better
series.names.tfPADIdata <- function(data) {dimnames(data)[[2]]}
# nseries default should work


identifiers.tfPADIdata <- function(obj)  {obj[1,]}
sourceserver.tfPADIdata <- function(obj)  {obj[2,]}
sourcedb.tfPADIdata <- function(obj)  {obj[3,]}
source.info.tfPADIdata <- function(obj)  {attr(obj,"source")} #used by refresh


"[.tfPADIdata" <- function (x, i, j, drop = FALSE) #N.B. FALSE
   {a <- attributes(x)
    y <- NextMethod("[")
    a$dim      <- dim(y)
    a$dimnames <- dimnames(y)
    attributes(y) <- a
    y
   }

tsp.tfPADIdata <- function(x)
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

availability.tfPADIdata <- function(data, verbose=T, timeout=60)  
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

freeze.FAMEdata <- function(data)
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
