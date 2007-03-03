% levelsBySubset.Rd
%--------------------------------------------------------------------------
% What: Get levels by disconnected subsets
% $Id: levelsBySubset.Rd 77 2007-03-03 18:00:55Z ggorjan $
% Time-stamp: <2007-03-03 18:59:42 ggorjan>
%--------------------------------------------------------------------------

\name{levelsBySubset}

\alias{levelsBySubset}

\title{Get levels by disconnected subsets}

\description{

\code{levelsBySubset} helps you to retrieve levels of disconnected subsets.

}

\usage{levelsBySubset(x, subset=NULL, factor="xy")}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{subset}{integer, subset number(s)}
  \item{factor}{character, which factor levels should we print, "x", "y",
    or "xy"}
}

\value{

A list with factor levels within list of disconnected subset(s).

}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{connectedness}}, \code{\link{subset.connectedness}} and
  \code{\link{plot.connectedness}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)
tmpD <- connectedness(x=connect$group, y=connect$season, drop=TRUE)

## levelBySubset method
levelsBySubset(x=tmp)
levelsBySubset(x=tmpD)
levelsBySubset(x=tmp, subset=1, factor="x")
levelsBySubset(x=tmp, subset=c(1, 2), factor="y")

}

\keyword{misc}

%--------------------------------------------------------------------------
% levelsBySubset.Rd ends here
