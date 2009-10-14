\name{tfstart}

\alias{tfstart}
\alias{tfstart.default}
\alias{tfstart.tstframe}
\alias{start.tframed}
\alias{start.tframe}

\alias{tfend}
\alias{tfend.default}
\alias{tfend.tstframe}
\alias{end.tframed}
\alias{end.tframe}

\alias{tffrequency}
\alias{tffrequency.default}
\alias{frequency.tframed}
\alias{frequency.tframe}

\alias{tfperiods}
\alias{tfperiods.default}
\alias{tfperiods.stamped}
\alias{periods}
\alias{periods.default}
\alias{periods.tframed}
\alias{periods.tframe}


\alias{tftime}
\alias{tftime.default}
\alias{time.tframed}
\alias{time.tframe}

\title{Extract Time Frame Information}
\description{Functions for extracting time frame information.}

\usage{
    \method{start}{tframed}(x, ...)
    \method{start}{tframe}(x, ...)
    tfstart(x)
    \method{tfstart}{default}(x)
    \method{tfstart}{tstframe}(x)

    \method{end}{tframed}(x, ...)
    \method{end}{tframe}(x, ...)
    \method{tfend}{tstframe}(x)
    tfend(x)
    \method{tfend}{default}(x)

    \method{frequency}{tframed}(x, ...)
    \method{frequency}{tframe}(x, ...)
    tffrequency(x)
    \method{tffrequency}{default}(x)

    periods(x)
    \method{periods}{default}(x)
    \method{periods}{tframed}(x)
    \method{periods}{tframe}(x)
    tfperiods(x)
    \method{tfperiods}{default}(x)
    \method{tfperiods}{stamped}(x)

    \method{time}{tframed}(x, ...)
    \method{time}{tframe}(x, ...)
    tftime(x)
    \method{tftime}{default}(x)
    \method{time}{tframed}(x, ...)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{...}{arguments to be passed to other methods.}
}
\value{Depends}
\details{
    The methods start and end return the start or end date of a tframe or
    tframed object. Periods return the number of observations (time points).
    frequency returns the frequency of observation, 
    typically the number of observations in a year for economic data, but
    possibly something else in other contexts.  The concept of frequency is
    not very consistently defined for time series data, and the use of 
    the frequency method should probably be avoided where possible. In
    practice it seems rarely necessary, but the method makes porting of 
    older code much easier. 
}

\seealso{
    \code{\link{tframe}}, 
    \code{\link{tframed}}
    \code{\link[stats]{start}}
    \code{\link[stats]{end}}
    \code{\link[stats]{frequency}}
    \code{\link{periods}}
    \code{\link[stats]{time}}
    \code{\link{lag}}
    \code{\link{diff}}
}
\examples{
    z <- ts(rnorm(100), start=c(1982,1), frequency=12)
    tfstart(z)
    z <- tframed(matrix(rnorm(200), 100,2), 
            tf=list(start=c(1982,1), frequency=12))
    tfend(z)
    periods(z)
    time(z)
}

\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{chron}

