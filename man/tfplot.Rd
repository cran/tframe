\name{tfplot}
\alias{tfplot}
\alias{tfplot.default}
\alias{tfOnePlot}

\title{Plot Tframed Objects}
\description{
    Plot tframe or tframed objects. 
}
\usage{
    tfplot(x, ...)

    \method{tfplot}{default}(x, ..., tf=tfspan(x, ...), start=tfstart(tf), end=tfend(tf),
       series=seq(nseries(x)), Title=NULL, 
        lty = 1:5, lwd = 1, pch = NULL, col = 1:6, cex = NULL,
       xlab=NULL, ylab=seriesNames(x), xlim = NULL, ylim = NULL, 
       graphs.per.page=5, par=NULL, mar=par()$mar, reset.screen=TRUE)
    tfOnePlot(x, tf=tframe(x), start=tfstart(tf), end=tfend(tf),
        Title=NULL, lty=1:5, lwd=1, pch=NULL, col=1:6, cex=NULL,
        xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, par=NULL,
	lastObs=FALSE,  source=NULL, footnote=NULL,
	legend=NULL, legend.loc="topleft")
}

\arguments{
    \item{x}{a tframe or tframed object to plot.}
    \item{...}{any additional tframed objects for the same plot.}
    \item{start}{start of plot. (passed to tfwindow)}
    \item{end}{end of plot. (passed to tfwindow)}
    \item{tf}{a tframe or tframed object which can be used to specify start and
    end.}
    \item{series}{series to be plotted. (passed to selectSeries)}
    \item{Title}{string to use for plot title (but see details).}
    \item{lty}{passed to plot. See also par.)}
    \item{lwd}{passed to plot. See also par.)}
    \item{pch}{passed to plot. See also par.)}
    \item{col}{passed to plot. See also par.)}
    \item{cex}{passed to plot. See also par.)}
    \item{xlab}{string to use for x label (passed to plot).}
    \item{ylab}{string to use for y label (passed to plot).}
    \item{xlim}{passed to plot. See also par.)}
    \item{ylim}{passed to plot. See also par.)}
    \item{lastObs}{Logical indicating if the date of the last observation
        should be printed below the graph, flushed right.}
    \item{source}{String printed below the graph, flushed left.}
    \item{footnote}{String printed below lastObs and source, flushed left.}
    \item{legend}{NULL (indicating no legend) or a vector strings to be used
       for a legend (see legend)}
    \item{legend.loc}{indication of placement of the legend (see legend)}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{par}{a list of arguments passed to par() before plotting.)}
    \item{mar}{margins passed to plot (deprecated, use par).)}
    \item{reset.screen}{logical indicating if the plot window should be cleared
       before starting. If this is not TRUE then mar values will have no effect.}
}
\value{None.}
\details{
    In many cases these are the same as plot methods, however, tfplot puts
    different series in the object \code{x} in different plot panels, whereas
    \code{plot} usually puts them in the same panel. For this reason,
    \code{tfplot} tends to work better when the scale of the different series
    are very different. If additional objects are supplied, then they should
    each have the same number of series as \code{x} and all corresponding series
    will be plotted in the same panel.
    
    \code{tfplot} provides an alternate generic
    mechanism for plotting time series data.
    New classes of time series may define
    there own \code{tfplot} (and \code{plot}) methods.
    
    The start and end arguments to tfplot 
    determine the start and end of the plot. The argument tf is an alternate way
    to specify the start and end. It is ignored if start and end are specified.

    If \code{xlim} and \code{ylim} are not NULL they should be a vector of two
    elements giving the max and min, which are applied to all graphs, or a list
    of length equal to the number of series to be plotted with each list element
    being the two element vector for the corresponding plot limits. 

    The \code{Title} is not put on the plot if the global option
    PlotTitles is FALSE. This can be set with \code{options(PlotTitles=FALSE)}. 
    This provides a convenient  mechanism to omit all titles when the title
    may be added separately (e.g. in Latex).
    
    Similarly, \code{options(PlotSource=FALSE)} and 
    \code{options(Plotfootnote=FALSE) can be used to suppress printing of 
    these.}
}
\section{Side Effects}{An object is plotted.}
\seealso{
    \code{\link{tfprint}},
    \code{\link{tframe}},
    \code{\link{tframed}},
    \code{\link{print}},
    \code{\link{plot}},
    \code{\link{legend}},
    \code{\link{par}}
}
\examples{
    tfplot(ts(rnorm(100), start=c(1982,1), frequency=12))
    tfplot(ts(rnorm(100), start=c(1982,1), frequency=12), start=c(1985,6))
}
\keyword{programming}
\keyword{utilities}
\keyword{ts}
\keyword{hplot}


