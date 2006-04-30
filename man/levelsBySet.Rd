% levelsBySet.Rd
%--------------------------------------------------------------------------
% What: Get levels by disconnected sets
% $Id$
% Time-stamp: <2006-05-02 02:31:11 ggorjan>
%--------------------------------------------------------------------------

\name{levelsBySet}

\alias{levelsBySet}

\title{Get levels by disconnected sets}

\description{
  \code{levelsBySet} helps you to retrieve levels of disconnected sets.   
}

\usage{levelsBySet(x, set=NULL, factor="xy")}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{set}{integer, set number(s)}
  \item{factor}{character, which factor levels should we print, "x", "y",
    or "xy"}
}

\value{
  A list with factor levels within list of disconnected set(s).   
}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{connectedness}}, \code{\link{levelsBySet}} and
  \code{\link{plot.connectedness}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)

## levelBySet method
levelsBySet(x=tmp)
levelsBySet(x=tmp, set=1, factor="x")
levelsBySet(x=tmp, set=c(1, 2), factor="y")

}

\keyword{misc}
\keyword{manip}

%--------------------------------------------------------------------------
% levelsBySet.Rd ends here
