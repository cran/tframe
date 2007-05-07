 require("stats")
 require("tframe")

 Sys.info()

z <- ts(matrix(100 + rnorm(200),100,2), start=c(1991,1), frequency=4)
tsWrite(z, file="tmp.test.data.csv")
zz <- tsScan("tmp.test.data.csv", nseries=2)


cat("max difference ", max(abs(z - zz)) )
if (max(abs(z - zz)) > 1e-10)   stop("file write and read comparison failed.")

file.remove("tmp.test.data.csv")

if ( !all(1 == (ts(1:5) - tfL(ts(1:5)))))
       stop("default test of tfL failed.")

if ( !all(2 == (ts(1:5) - tfL(ts(1:5), p= 2))))
       stop("2 period lag test of tfL failed.")

z <- ts(1:10, start=c(1992,1), frequency=4)
if ( !all(1 == (z - tfL(z)))) stop("frequency=4 test of tfL failed.")

z <- ts(matrix(1:10,5,2), start=c(1992,1), frequency=4)
seriesNames(z) <- c("One", "Two")
if ( !all(1 == (z - tfL(z)))) stop("matrix test of tfL failed.")
