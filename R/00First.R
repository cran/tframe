.First.lib <- function(library,section){
	if(!require("syskern",  warn.conflicts=F)) 
	warning("This package requires the syskern package.") # only for is.S ?
	invisible()
}
