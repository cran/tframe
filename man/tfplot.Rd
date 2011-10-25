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
       series=seq(nseries(x)), 
        Title=NULL, title=Title, subtitle=NULL, 
        lty = 1:5, lwd = 1, pch = NULL, col = 1:6, cex = NULL,
       xlab=NULL, ylab=seriesNames(x), xlim = NULL, ylim = NULL, 
       graphs.per.page=5, par=NULL, mar=par()$mar, reset.screen=TRUE, 
       lastObs = FALSE, source = NULL,
       footnote = NULL, footnoteLeft = footnote, footnoteRight = NULL)
    tfOnePlot(x, tf=tframe(x), start=tfstart(tf), end=tfend(tf),
        Title=NULL, title=Title,  subtitle=NULL, 
	lty=1:5, lwd=1, pch=NULL, col=1:6, cex=NULL,
        xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, par=NULL,
	lastObs=FALSE,  source=NULL, 
	footnote=NULL, footnoteLeft=footnote, footnoteRight=NULL,
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
    \item{title}{string to use for plot title (but see details).}
    \item{Title}{synonym for title.}
    \item{subtitle}{string to use for plot subtitle (but see details).}
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
    \item{footnote}{Synonym for footnoteLeft.}
    \item{footnoteLeft}{String printed below lastObs and source, flushed left.}
    \item{footnoteRight}{String printed below lastObs and source, flushed right.}
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
    
    \code{tfplot} provides an alternate generic mechanism for plotting time
    series data. New classes of time series may define
    there own \code{tfplot} (and \code{plot}) methods.
    
    
    \code{tfplot} does calls to \code{tfOnePlot} for each panel.
    \code{tfOnePlot} may give slightly better control, especially in cases
    where all series are to go on one plot. The functions are intended to 
    provide a convenient way to do some usual things. Ultimately
    \code{tfOnePlot} calls \code{plot}, \code{title}, and \code{mtext}, so
    even more control of plot details can be acheived by calling those
    functions directly.
    
    The \code{start} and \code{end} arguments to \code{tfplot} 
    determine the start and end of the plot. The argument \code{tf} is an
    alternate way to specify the start and end. It is ignored 
    if \code{start} and \code{end} are specified.

    If \code{xlim} and \code{ylim} are not NULL they should be a vector of two
    elements giving the max and min, which are applied to all graphs, or a list
    of length equal to the number of series to be plotted with each list element
    being the two element vector for the corresponding plot limits. 

    The \code{title} is not put on the plot if the global option
    PlotTitles is FALSE. This can be set with \code{options(PlotTitles=FALSE)}. 
    This provides a convenient  mechanism to omit all titles when the title
    may be added separately (e.g. in Latex).
    
    Similarly, \code{options(PlotPlotSubtitles=FALSE)},
    \code{options(PlotSources=FALSE)}, and
    \code{options(PlotFootnotes=FALSE) can be used to suppress printing of 
    these.}
    
    Footnotes can contain \code{"\n"} to produce multiline, or multiple
    footnotes. However, if  \code{source} and \code{lastObs} are speciied then
    the overlap can be messy. In this case a better result might be obtained by
    specifying the source as part of the footnote.
    
    If \code{subtitle}, \code{source}, \code{footnoteLeft} or
    \code{footnoteRight} have length less than the number of panels then they
    are replicated, so typically they should have one element that is applied 
    to each panel, or be vectors with one element for each panel.
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


