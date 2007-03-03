% plot.connectedness.Rd
%--------------------------------------------------------------------------
% What: Plot disconnected subsets in a two-way classification
% $Id: plot.connectedness.Rd 80 2007-03-04 11:52:04Z ggorjan $
% Time-stamp: <2007-03-03 18:31:51 ggorjan>
%--------------------------------------------------------------------------

\name{plot.connectedness}

\alias{plot.connectedness}

\title{Plot disconnected subsets in a two-way classification}

\description{

\code{plot.connectedness} plots disconnected subsets in a two-way
classification without interaction.

}

\usage{
plot.connectedness(x, subset=NULL, matrix=TRUE, scale=TRUE,
                   lines=FALSE, linesSubset=NULL,
                   col=NULL,
                   plotArg=list(xlab=x$factors[1],
                                ylab=x$factors[2]),
                   polygonArg=NULL,
                   pointsArg=list(pch=rep(19, x$nSubsets)),
                   linesArg=NULL, \ldots)
}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{subset}{integer, which subset to plot, all subsets if NULL}
  \item{matrix}{logical, if TRUE, plot a matrix of polygons, otherwise
    plot points}
  \item{scale}{logical, if TRUE, scale axes in relation to number of
    records per each level, otherwise not}
  \item{lines}{logical, connect cells with lines within subsets}
  \item{linesSubset}{numeric, subset number i.e. which subsets should have
    connected cells}
  \item{col}{character, valid color "names" for cell polygons or points,
    see details}
  \item{plotArg}{list, arguments passed to \code{plot}}
  \item{polygonArg}{list, arguments passed to \code{polygon}, see details}
  \item{pointsArg}{list, arguments passed to \code{points} if
    matrix=FALSE, see details}
  \item{linesArg}{list, arguments passed to \code{lines} if lines=TRUE,
    see details}
  \item{\ldots}{arguments for other methods, not used}
}

\details{

Colors for polygons, points and lines can be given with \code{col} for
all elements or separately within \code{ploygonArg}, \code{pointsArg}
and \code{linesArg}. By default \code{col} is NULL and colors are given
internally. Colors are taken from \pkg{RColorBrewer} package,
specifically from qualitative palettes. Read help page of
\code{\link[RColorBrewer]{brewer.pal}} for more. Only 54 colors are
defined and an error is issued, if there are more than 54 subsets in
your data (sigh!). You should define colors by yourself in that case. For a
start you can access 54 internal colors with
\code{connectedness:::.colors}.

Length of all arguments in \code{ploygonArg}, \code{pointsArg} and
\code{linesArg} is assumed to be equal to the number of subsets in
\code{x}. If it is less, recycling will occur. Therefore, result might
be a mess, unless lengths of arguments are 1. In last case all subsets
will have equal value for the corresponding argument(s).

Plot does not handle \code{drop} slot in \code{x} and draws cells only
for used levels. This might change if I get a got idea how to plot
"missing" cels ;)

}

\author{Gregor Gorjanc (colors were taken from RColorBrewer package)}

\seealso{
  \code{\link{connectedness}}, \code{\link{levelsBySubset}} and
  \code{\link{subset.connectedness}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)

## Plot method
par(mfrow=c(2, 2))
plot(tmp)

plot(tmp, matrix=FALSE, lines=TRUE, col=c("green", "blue"),
     pointsArg=list(pch=c(15, 19), cex=2), linesArg=list(lwd=2))

plot(tmp, scale=FALSE, lines=TRUE, linesSubset=1,
     linesArg=list(col="black", lwd=2))

plot(tmp, subset=2, col=c("gray"),
     plotArg=list(xlab="Group", ylab="Season"))
}

\keyword{hplot}

%--------------------------------------------------------------------------
% plot.connectedness.Rd ends here
