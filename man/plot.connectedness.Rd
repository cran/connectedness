% plot.connectedness.Rd
%--------------------------------------------------------------------------
% What: Plot disconnected sets for two-way classification
% $Id$
% Time-stamp: <2006-05-02 02:01:31 ggorjan>
%--------------------------------------------------------------------------

\name{plot.connectedness}

\alias{plot.connectedness}

\title{Plot disconnected sets for two-way classification}

\description{
  \code{plot.connectedness} plots disconnected sets for two-way
  classification without interaction.   
}

\usage{
plot.connectedness(x, set=NULL, matrix=TRUE, scale=TRUE,
                   lines=FALSE, linesSet=NULL,
                   col=NULL,
                   plotArg=list(xlab=x$factors[1],
                                ylab=x$factors[2]),
                   polygonArg=NULL,
                   pointsArg=list(pch=rep(19, x$nSets)),
                   linesArg=NULL, ...)
}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{set}{integer, which set to plot, all sets if NULL}
  \item{matrix}{boolean, if TRUE, plot a matrix of polygons, otherwise
    plot points}
  \item{scale}{boolean, if TRUE, scale axes in relation to number of
    records per each level, otherwise not}
  \item{lines}{boolean, connect cells with lines within sets}
  \item{linesSet}{numeric, set number i.e. which sets should have
    connected cells}
  \item{col}{character, valid color "names" for cell polygons or points,
    see details}
  \item{plotArg}{list, arguments passed to \code{plot}}
  \item{polygonArg}{list, arguments passed to \code{polygon}, see details}
  \item{pointsArg}{list, arguments passed to \code{points} if
    matrix=FALSE, see details}
  \item{linesArg}{list, arguments passed to \code{lines} if lines=TRUE,
    see details}
  \item{...}{arguments for other methods, not used}
}

\details{
  Colors for polygons, points and lines can be given with \code{col}
  or separately with \code{ploygonArg}, \code{pointsArg} and
  \code{linesArg}. By default \code{col} is NULL and colors are given
  internally. Set of colors is taken from \pkg{RColorBrewer} package,
  specifically from qualitative palettes. Read help page of
  \code{\link[RColorBrewer]{brewer.pal}} for more. Only 54 colors are
  defined and an error is issued, if there are more than 54 sets in your
  data. You should define colors by yourself in that case. For a start
  you can access 54 internal colors with \code{connectedness:::.colors}.

  Length of all arguments in \code{ploygonArg}, \code{pointsArg} and
  \code{linesArg} is assumed to be equal to then number of sets in
  \code{x}. If it is less, recycling will occur. Therefore, result might
  be a mess, unless length of arguments is 1. In last case all set
  will have equal value for the corresponding argument(s).   
}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{connectedness}}, \code{\link{levelsBySet}} and
  \code{\link{subset.connectedness}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)

## Plot method
par(mfrow=c(2, 2))
plot(tmp)

plot(tmp, matrix=FALSE, lines=TRUE, col=c("red", "blue"),
     pointsArg=list(pch=c(15, 19), cex=2), linesArg=list(lwd=2))

plot(tmp, scale=FALSE, lines=TRUE, linesSet=1,
     linesArg=list(col="black", lwd=2))

plot(tmp, set=2, col=c("gray"),
     plotArg=list(xlab="Group", ylab="Season"))
}

\keyword{hplot}
\keyword{manip}

%--------------------------------------------------------------------------
% plot.connectedness.Rd ends here
