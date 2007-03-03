### connectedness.R
###------------------------------------------------------------------------
### What: Find disconnected subsets for two-way classification
### $Id: connectedness.R 80 2007-03-04 11:52:04Z ggorjan $
### Time-stamp: <2007-03-03 21:30:05 ggorjan>
###------------------------------------------------------------------------

connectedness <- function(x, y, sort=TRUE, subsetData=TRUE, drop=FALSE)
{
  ## --- Checks & Setup ---

  lengthX <- length(x)
  if(lengthX != length(y))
    stop("'x' and 'y' must have the same length")

  xLab <- deparse(substitute(x))
  yLab <- deparse(substitute(y))

  ## --- Remove NA's and unused levels ---

  full <- complete.cases(x, y)

  if(drop) {
    if(lengthX != sum(full)) {
      x <- x[full]
      y <- y[full]
      if(is.factor(x)) x <- factor(x) # \ drop unused levels if these
      if(is.factor(y)) y <- factor(y) # / are factors
      lengthX <- length(x)
    }
  }

  ## --- Create table ---

  tab <- table(x, y)
  h <- tab
  cols <- ncol(h)
  rows <- nrow(h)
  for(i in 2:cols) {
    h[h[, i] > 0, i] <- i
  }

  ## --- Find subsets ---

  cSubset <- rep(0, times=nrow(h))
  for(i in 1:cols) {
    cSubset <- cSubset + h[, i]
    test <- cSubset > i
    if(any(test)) {
      d <- cSubset[test]
      cSubset[test] <- i
      test <- cSubset %in% (d - i)
      if(any(test)) cSubset[test] <- i
    }
  }

  ## --- Output ---

  subsets <- unique(cSubset)
  nSubsets <- length(subsets)

  ret <- vector(mode="list", length=7)
  names(ret) <- c("factors", "nlevels", "nSubsets", "subsets", "table", "subsetData", "drop")
  ret$factors <- c(xLab, yLab)
  ret$nlevels <- c(rows, cols)
  ret$nSubsets <- nSubsets
  ret$subsets <- data.frame(Subset=1:nSubsets, Freq=0, Percent=0, Levels1=NA, Levels2=NA)
  ret$table <- as.data.frame(tab)
  if(subsetData) {
    ret$subsetData <- data.frame(keep=full, subset=1)
  } else {
    ret$subsetData <- NA
  }
  ret$drop <- drop
  ret$table <- ret$table[!(ret$table$Freq == 0), ]
  ret$table$subset <- 1
  rownames(ret$table) <- 1:nrow(ret$table)

  for(i in seq(along=subsets)) {
    tmp <- names(cSubset)[cSubset == subsets[i]]
    tmp1 <- ret$table[ret$table$x %in% tmp, "y"]
    ## | due to possible NA
    ret$subsets[i, "Freq"] <- sum(x %in% tmp | y %in% tmp1)
    ret$subsets[i, "Percent"] <- ret$subsets[i, "Freq"] / lengthX * 100
    ret$subsets[i, "Levels1"] <- paste(unique(tmp), collapse=" ")
    ret$subsets[i, "Levels2"] <- paste(unique(tmp1), collapse=" ")
    ## | due to possible NA
    ret$table[(ret$table$x %in% tmp | ret$table$y %in% tmp1), "subset"] <- i
    if(subsetData) {
      ret$subsetData[(x %in% tmp), "subset"] <- i
    }
  }
  if(sort) {
    ret$subsets <- ret$subsets[order(ret$subsets$Freq, decreasing=TRUE), ]
    row.names(ret$subsets) <- 1:nSubsets
  }

  class(ret) <- c("connectedness", class(ret))
  ret
}

is.connectedness <- function(x)
  inherits(x=x, what="connectedness")

###------------------------------------------------------------------------
### connectedness.R ends here
