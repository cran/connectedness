### subset.connectedness.R
###------------------------------------------------------------------------
### What: Subset method
### $Id: subset.connectedness.R 80 2007-03-04 11:52:04Z ggorjan $
### Time-stamp: <2007-03-03 19:25:17 ggorjan>
###------------------------------------------------------------------------

subset.connectedness <- function(x, data, ..., subset=NULL, dropNA=NULL)
{
  if(!is.data.frame(x$subsetData)) stop("'subsetData' slot in 'x' is empty")
  if(is.null(subset)) subset <- x$subsets$Subset ## all subsets
  if(is.null(dropNA)) dropNA <- x$drop
  test <- x$subsetData$subset %in% subset
  if(dropNA) {
    ind <- x$subsetData$keep
    test <- test[ind]
  } else {
    ind <- 1:nrow(data)
  }
  ## argument subset in upper function definition is after ... and
  ## therefore does not go into call of subset() bellow
  subset(data[ind, ], subset=test, ...)
}

###------------------------------------------------------------------------
### subset.connectedness.R ends here
