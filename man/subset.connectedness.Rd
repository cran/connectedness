% subset.connectedness.Rd
%--------------------------------------------------------------------------
% What: Subset disconnected sets
% $Id$
% Time-stamp: <2006-05-01 22:48:42 ggorjan>
%--------------------------------------------------------------------------

\name{subset.connectedness}

\alias{subset.connectedness}

\title{Subset disconnected sets}

\description{
  \code{subset.connectedness} helps you to subset data based on
  disconnected sets.   
}

\usage{subset.connectedness(x, data, set=NULL, ...)}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{data}{data.frame, data}
  \item{set}{integer, set number(s), see details}
  \item{...}{argument passed to \code{subset.data.frame}}
}

\details{
  \code{set} defines which sets of records will stay in output of this
  function. If \code{set=NULL} "all" records will be returned. If there
  were any NA's in call of \code{\link{connectedness}} not exactly all
  reccords will be returned here because \code{\link{connectedness}}
  excludes records with NA's (subject to change/improve in future).

  If subset slot is NULL, the following error is issued "subset slot
  NULL". This is due to use of argument \code{subset=FALSE} in
  \code{\link{connectedness}}.   
}

\value{
  Data.frame with records that correspond to defined sets in
  \code{set}.   
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

## Subset method
subset(x=tmp, data=connect, set=1)
subset(x=tmp, data=connect, set=2)
subset(x=tmp, data=connect, set=c(1, 2))
subset(x=tmp, data=connect)

}

\keyword{misc}
\keyword{manip}

%--------------------------------------------------------------------------
% subset.connectedness.Rd ends here
