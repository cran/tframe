\name{freeze}
\alias{freeze}
\alias{freeze.default}
\alias{freeze.FAMEdata}
\title{Get fixed data snapshot}
\description{Generic method. See specific methods for details.}
\details{
This function extracts data from 
a database (for example using the TS PADI programs are available 
at <www.bank-banque-canada.ca/pgilbert> ). This method is generic.
Typically the argument data is a tfPADIdata or TSPADIdata object identifying 
the source of 
the data. See help for tfPADIdata and TSPADIdata. 
The default method usually just
returns its argument, so freeze has no effect. This way freeze
can be used to write functions which will take a snapshot from the database
when they execute or will work with an already fixed copy of data if that is
what is supplied. The default does allow for a character argument, in which case
it is used to construct a tfPADIdata object using server="ets", then freeze that
object. This allows for a simple syntax to grab a series from the database. The
server="ets" is for convenience at the Bank of Canada and another default server
might be more convenient elsewhere.
}

\seealso{ 
	\code{\link{tfPADIdata}}
	\code{\link{tfputpadi}}
	\code{\link[dse2]{freeze.tfPADIdata}}
	\code{\link[dse2]{freeze.TSPADIdata}}
	\code{\link[dse2]{TSPADIdata}}
}
\keyword{tframe}
\keyword{time series data}

