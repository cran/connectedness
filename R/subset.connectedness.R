## subset.connectedness.R
###------------------------------------------------------------------------
## What: Subset method
## $Id$
## Time-stamp: <2006-05-01 22:44:46 ggorjan>
###------------------------------------------------------------------------

subset.connectedness <- function(x, data, set=NULL, ...)
{
  if (is.null(x$subset)) stop("subset slot NULL")
  if (is.null(set)) set <- x$sets$Set
  test <- x$subset$set %in% set
  subset(data[rownames(x$subset), ], subset=test, ...)
}

###------------------------------------------------------------------------
## subset.connectedness.R ends here
