## levelsBySet.R
###------------------------------------------------------------------------
## What: Get levels by disconnected sets
## $Id$
## Time-stamp: <2006-05-02 02:20:21 ggorjan>
###------------------------------------------------------------------------

levelsBySet <- function(x, set=NULL, factor="xy")
{
  if (!("connectedness" %in% class(x)))
    stop("can be used only on output of connectedness()")
  if (!is.null(set)) {
    x <- x$sets[set, ]
  } else {
    x <- x$sets
  }
  tmp <- switch(factor,
                x = "Levels1",
                y = "Levels2",
                xy = c("Levels1", "Levels2"))
  y <- x[, tmp]
  if (factor == "xy") {
    ret <- apply(y, 1, strsplit, split=" ")
  } else {
    ret <- lapply(strsplit(y, split=" "), list)
    names(ret) <- x$Set
    ret <- lapply(ret, "names<-", tmp)
  }
  return(ret)
}

###------------------------------------------------------------------------
## levelsBySet.R ends here
