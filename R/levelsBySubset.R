### levelsBySubset.R
###------------------------------------------------------------------------
### What: Get levels by disconnected subsets
### $Id: levelsBySubset.R 80 2007-03-04 11:52:04Z ggorjan $
### Time-stamp: <2007-03-03 21:07:08 ggorjan>
###------------------------------------------------------------------------

levelsBySubset <- function(x, subset=NULL, factor="xy")
{
  if(!inherits(x=x, what="connectedness"))
    stop(sprintf("can be used only on %s class - output of 'connectedness()'",
                 dQuote("connectedness")))
  if(!is.null(subset)) {
    x <- x$subsets[x$subsets$Subset %in% subset, ]
  } else {
    x <- x$subsets
  }
  tmp <- switch(factor,
                x="Levels1",
                y="Levels2",
                xy=c("Levels1", "Levels2"))
  y <- x[, tmp]
  if(factor == "xy") {
    ret <- apply(y, 1, strsplit, split=" ")
    names(ret) <- x$Subset
  } else {
    ret <- lapply(strsplit(y, split=" "), list)
    names(ret) <- x$Subset
    ret <- lapply(ret, "names<-", tmp)
  }
  ret
}

###------------------------------------------------------------------------
### levelsBySubset.R ends here
