% connectedness.Rd
%--------------------------------------------------------------------------
% What: Find disconnected subsets in a two-way classification
% $Id: connectedness.Rd 80 2007-03-04 11:52:04Z ggorjan $
% Time-stamp: <2007-03-03 21:33:48 ggorjan>
%--------------------------------------------------------------------------

\name{connectedness}

\alias{connectedness}
\alias{print.connectedness}

\title{Find disconnected subsets in a two-way classification}

\description{

\code{connectedness} finds disconnected subsets in a two-way
classification without interaction. Its output can be used for plotting
and subseting the data.

}

\usage{connectedness(x, y, sort=TRUE, subsetData=TRUE, drop=FALSE)}

\arguments{
  \item{x}{vector, factor1}
  \item{y}{vector, factor2}
  \item{sort}{logical, sort subsets by number of records}
  \item{subsetData}{logical, fill the \code{subsetData} slot, see details}
  \item{drop}{logical, drop unused levels, see details}
}

\details{

\code{x} and \code{y} must be vectors of the same length. Class of
\code{x} and \code{y} need not be factor, however this is the most
common usage.

Only complete cases (via \code{complete.cases}) of \code{x} and \code{y}
are processed i.e. combinations with NA are removed.

List structure of returned value is:
\itemize{
  \item factors: character, factor names
  \item nlevels: integer, number of levels for each factor
  \item nsubsets: integer, number of disconected subsets
  \item subsets: data.frame, subset characteristics - used in print method;
  with following columns:
  \itemize{
    \item Subset: integer, sequential number of a subset
    \item Freq: integer, number of record in correseponding subset
    \item Percent: numeric, percentage of records in correseponding subset
    \item Levels: character, level names in correseponding subset
  }
  \item table: data.frame, observed combinations of factors - used in
  plot method; with following columns:
  \itemize{
    \item x: factor, factor1 level
    \item y: factor, factor2 level
    \item Freq: integer, number of records for combination of factors
    \item subset: integer, sequential number of a subset
  }
  \item subsetData: data.frame, used in subset method; with following
  columns:
  \itemize{
    \item keep: logiacl, value indicating which row should be removed if
      \code{NA} are to be removed, used for subset method
    \item subset: integer, sequential number of a subset
  }
  \item drop: logical, single value indicating whether subset method
  should drop or keep unused levels - see help page for subset method.
}

If you work with big datasets, \code{subsetData=FALSE} might help you in
returning smaller object. However, \code{subset} method can not be used
on such "truncated" object; read also help page of
\code{\link{subset.connectedness}}.

Argumnet \code{drop} can be used to drop unused levels before finding
subsets. This will remove rows with either factor having missing values
(\code{NA}).

In case of many levels print method truncates them to keep print within
\code{getOption("width")}. Use \code{\link{levelsBySubset}} to get all
levels by subset.

}

\value{

A list of class "connectedness" and "list" returned with structure as
described in details.

}

\author{Gregor Gorjanc}

\references{

Fernando, R. L. and Gianola, D. and Grossman, M. (1983) Identify all
connected subsets in a two-way classification without interaction.
Journal of Dairy Science, 66:1399-1402

Searle, S.R. (1987) Linear models for unbalanced models. John Wiley and
Sons Inc.

}

\seealso{
  \code{\link{plot.connectedness}},
  \code{\link{levelsBySubset}} and
  \code{\link{subset.connectedness}}; similar functionality is available
  in \pkg{pheno} package: \code{\link[pheno]{connectedSets}} and
  \code{\link[pheno]{maxConnectedSet}}
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)
tmpD <- connectedness(x=connect$group, y=connect$season, drop=TRUE)

## Print method
tmp
print(tmp)
print(tmpD)

## Plot method
par(mfrow=c(2, 2))
plot(tmp)

plot(tmp, matrix=FALSE, lines=TRUE, col=c("green", "blue"),
     pointsArg=list(pch=c(15, 19), cex=2), linesArg=list(lwd=2))

plot(tmp, scale=FALSE, lines=TRUE, linesSubset=1,
     linesArg=list(col="black", lwd=2))

plot(tmp, subset=1, col=c("gray"),
     plotArg=list(xlab="Group", ylab="Season"))

## Subset method
subset(x=tmp, data=connect, subset=1)
subset(x=tmpD, data=connect, subset=1)
subset(x=tmp, data=connect, subset=2)
subset(x=tmp, data=connect, subset=c(1, 2))
subset(x=tmp, data=connect)

## levelBySet method
levelsBySubset(x=tmp)
levelsBySubset(x=tmpD)
levelsBySubset(x=tmp, subset=1, factor="x")
levelsBySubset(x=tmp, subset=2, factor="xy")
levelsBySubset(x=tmp, subset=c(1, 2), factor="y")

}

\keyword{misc}

%--------------------------------------------------------------------------
% connectedness.Rd ends here
