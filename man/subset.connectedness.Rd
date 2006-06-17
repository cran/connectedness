% subset.connectedness.Rd
%--------------------------------------------------------------------------
% What: Subset disconnected sets
% $Id$
% Time-stamp: <2006-08-08 11:33:37 ggorjan>
%--------------------------------------------------------------------------

\name{subset.connectedness}

\alias{subset.connectedness}

\title{Subset disconnected sets}

\description{

\code{subset.connectedness} helps you to subset data based on
disconnected sets.

}

\usage{subset.connectedness(x, data, \ldots, set=NULL, dropNA=NULL)}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{data}{data.frame, data}
  \item{\ldots}{arguments passed to \code{subset.data.frame}}
  \item{set}{integer, set number(s), see details}
  \item{dropNA}{boolean, drop sets with missing levels, see details}
}

\details{

\code{set} defines which sets of records will stay in output of this
function. If \code{set=NULL} "all" records will be returned. If there
were any NA's in a call of \code{\link{connectedness}}, not exactly all
reccords will be returned here because \code{\link{connectedness}}
excludes records with NA's (subject to change/improve in future).

If subset slot in \code{x} is NULL, the following error is issued
"subset slot is empty". This is due to the use of argument
\code{subset=FALSE} in \code{\link{connectedness}}. You will have to
rerun \code{\link{connectedness}} with \code{subset=TRUE} to overcome
this issue.

Argument \code{dropNA} can be used to remove data.frame rows that have
unused levels for either factor that was used in
\code{\link{connectedness}}. By default value is taken from object
\code{x}.

}

\value{

Data.frame with records that correspond to defined sets in \code{set}.

}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{connectedness}}, \code{\link{levelsBySet}},
  \code{\link{plot.connectedness}} and \code{\link{subset}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)
tmpD <- connectedness(x=connect$group, y=connect$season, drop=TRUE)

## Subset method
subset(x=tmp, data=connect)
subset(x=tmpD, data=connect)
subset(x=tmp, data=connect, dropNA=TRUE)
subset(x=tmp, data=connect, set=1)
subset(x=tmp, data=connect, set=2)
subset(x=tmp, data=connect, set=c(1, 2))
}

\keyword{manip}

%--------------------------------------------------------------------------
% subset.connectedness.Rd ends here
