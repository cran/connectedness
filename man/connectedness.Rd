% connectedness.Rd
%--------------------------------------------------------------------------
% What: Find disconnected sets for two-way classification
% $Id$
% Time-stamp: <2006-05-02 02:24:04 ggorjan>
%--------------------------------------------------------------------------

\name{connectedness}

\alias{connectedness}
\alias{print.connectedness}

\title{Find disconnected sets for two-way classification}

\description{
  \code{connectedness} finds disconnected sets for two-way
  classification without interaction. Its output can be used for
  plotting and subseting the data.   
}

\usage{connectedness(x, y, sort=TRUE, subset=TRUE)}

\arguments{
  \item{x}{vector, factor1}
  \item{y}{vector, factor2}
  \item{sort}{boolean, sort sets by number of records}
  \item{subset}{boolean, fill the \code{subset} slot, see details}
}

\details{
  \code{x} and \code{y} must be vectors of the same length. Class of
  \code{x} and \code{y} need not be factor, however this is the most
  common usage.

  Only complete cases (via \code{complete.cases}) of \code{x} and
  \code{y} are processed i.e. combinations with NA are removed.

  List structure of returned value is:
  \itemize{
    \item factors: character, factor names
    \item nlevels: integer, number of levels for each factor
    \item nsets: integer, number of disconected sets
    \item sets: data.frame, set characteristics - used in print method;
    with following columns:
    \itemize{
      \item Set: integer, sequential number of a set
      \item Freq: integer, number of record in correseponding set
      \item Percent: numeric, percentage of records in correseponding set
      \item Levels: character, level names in correseponding set
    }
    \item table: data.frame, observed combinations of factors - used in
    plot method; with following columns:
    \itemize{
      \item x: factor, factor1 level
      \item y: factor, factor2 level
      \item Freq: integer, number of records for combination of factors
      \item set: integer, sequential number of a set
    }
    \item subset: data.frame, used in subset method; with following
    columns:
    \itemize{
      \item x: factor, original factor1
      \item set: integer, sequential number of a set
    }
  }

  If you work with big datasets, \code{subset=FALSE} might help you in
  returning smaller object. However, \code{subset} method can not be
  used on such "truncated" object; read also help page of
  \code{\link{subset.connectedness}}.

  In case of many levels print method truncates them to keep print
  within \code{getOption("width")}. Use \code{\link{levelsBySet}} to get
  all levels by set.   
}

\value{
  A list of class "connectedness" and "list" with the following entries
  is returned as described in details.
}

\author{Gregor Gorjanc}

\references{
  Fernando, R. L. and Gianola, D. and Grossman, M. (1983) Identify all
  connected subsets in a two-way classification without interaction.
  Journal of Dairy Science, 66:1399-1402

  Searle, S.R. (1987) Linear models for unbalanced models. John Wiley
  and Sons Inc.
}

\seealso{
  \code{\link{plot.connectedness}},
  \code{\link{levelsBySet}} and
  \code{\link{subset.connectedness}}; similar functionality is available
  in \pkg{pheno} package: \code{\link[pheno]{connectedSets}} and
  \code{\link[pheno]{maxConnectedSet}}   
}

\examples{

data(connect)
table(connect$group, connect$season)
tmp <- connectedness(x=connect$group, y=connect$season)

## Print method
tmp
print(tmp)
print(tmp, digits=2)

## Plot method
par(mfrow=c(2, 2))
plot(tmp)

plot(tmp, matrix=FALSE, lines=TRUE, col=c("red", "blue"),
     pointsArg=list(pch=c(15, 19), cex=2), linesArg=list(lwd=2))

plot(tmp, scale=FALSE, lines=TRUE, linesSet=1,
     linesArg=list(col="black", lwd=2))

plot(tmp, set=2, col=c("gray"),
     plotArg=list(xlab="Group", ylab="Season"))

## Subset method
subset(x=tmp, data=connect, set=1)
subset(x=tmp, data=connect, set=2)
subset(x=tmp, data=connect, set=c(1, 2))
subset(x=tmp, data=connect)

## levelBySet method
levelsBySet(x=tmp)
levelsBySet(x=tmp, set=1, factor="x")
levelsBySet(x=tmp, set=c(1, 2), factor="y")

}

\keyword{misc}
\keyword{manip}

%--------------------------------------------------------------------------
% connectedness.Rd ends here
