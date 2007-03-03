% subset.connectedness.Rd
%--------------------------------------------------------------------------
% What: Subset disconnected subsets
% $Id: subset.connectedness.Rd 80 2007-03-04 11:52:04Z ggorjan $
% Time-stamp: <2007-03-03 18:56:36 ggorjan>
%--------------------------------------------------------------------------

\name{subset.connectedness}

\alias{subset.connectedness}

\title{Subset data by disconnected subsets}

\description{

\code{subset.connectedness} helps you to subset data based on
disconnected subsets.

}

\usage{subset.connectedness(x, data, \ldots, subset=NULL, dropNA=NULL)}

\arguments{
  \item{x}{connectedness, output from \code{connectedness}}
  \item{data}{data.frame, data}
  \item{\ldots}{arguments passed to \code{subset.data.frame}}
  \item{subset}{integer, subset number(s), see details}
  \item{dropNA}{logical, drop subsets with missing levels, see details}
}

\details{

\code{subset} defines which subsets of records will stay in output of
this function. This value is used to compute value for \code{subset}
argument for default \code{subset} method for a data.frame. If
\code{subset=NULL} "all" records will be returned.

FIXME

If there are any \code{NA}'s in the data, a call of
\code{\link{connectedness}}, not exactly all records will be returned
here because \code{\link{connectedness}} excludes records with NA's
(subject to change/improve in future).

If subset slot in \code{x} is NULL, the following error is issued
"subset slot is empty". This is due to the use of argument
\code{subset=FALSE} in \code{\link{connectedness}} to get lean output in
case of big datasets. You will have to rerun \code{\link{connectedness}}
with \code{subset=TRUE} to overcome this issue.

Argument \code{dropNA} can be used to remove data.frame rows that have
unused levels for either factor that was used in
\code{\link{connectedness}}. By default value is taken from object
\code{x}.

}

\value{

Data.frame with records that correspond to defined subsets in
\code{subset}.

}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{connectedness}}, \code{\link{levelsBySubset}},
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
subset(x=tmp, data=connect, subset=1)
subset(x=tmp, data=connect, subset=2)
subset(x=tmp, data=connect, subset=c(1, 2))
}

\keyword{manip}

%--------------------------------------------------------------------------
% subset.connectedness.Rd ends here
