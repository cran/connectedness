## subset.connectedness.R
###------------------------------------------------------------------------
## What: Subset method
## $Id$
## Time-stamp: <2006-08-08 03:24:18 ggorjan>
###------------------------------------------------------------------------

subset.connectedness <- function(x, data, ..., set=NULL, dropNA=NULL)
{
  if(!is.data.frame(x$subset)) stop("subset slot is empty")
  if(is.null(set)) set <- x$sets$Set
  if(is.null(dropNA)) dropNA <- x$drop
  test <- x$subset$set %in% set
  if(dropNA) {
    ind <- x$subset$keep
    test <- test[ind]
  } else {
    ind <- 1:nrow(data)
  }
  return(subset(data[ind, ], subset=test, ...))
}

###------------------------------------------------------------------------
## subset.connectedness.R ends here
