## connectedness.R
###------------------------------------------------------------------------
## What: Find disconnected sets for two-way classification
## $Id$
## Time-stamp: <2006-05-01 23:47:32 ggorjan>
###------------------------------------------------------------------------

connectedness <- function(x, y, sort=TRUE, subset=TRUE)
{
  ## --- Checks ---

  lengthX <- length(x)
  if (lengthX != length(y))
    stop(paste(sQuote("x"), "and", sQuote("y"), "must have the same length"))

  xLab <- deparse(substitute(x))
  yLab <- deparse(substitute(y))

  ## --- Remove NA's and unused levels ---

  full <- complete.cases(x, y)
  if (lengthX != sum(full)) {
    x <- x[full]
    y <- y[full]
    if (is.factor(x)) x <- factor(x)
    if (is.factor(y)) y <- factor(y)
  }

  ## --- Create table ---

  tab <- table(x, y)
  h <- tab
  cols <- ncol(h)
  rows <- nrow(h)
  for (i in 2:cols) {
    h[h[, i] > 0, i] <- i
  }

  ## --- Find sets ---

  c <- rep(0, times=nrow(h))
  for (i in 1:cols) {
    c <- c + h[, i]
    test <- c > i
    if (any(test)) {
      d <- c[test]
      c[test] <- i
      test <- c %in% (d - i)
      if (any(test)) c[test] <- i
    }
  }

  ## --- Output ---

  sets <- unique(c)
  nSets <- length(sets)

  ret <- vector(mode="list", length=6)
  names(ret) <- c("factors", "nlevels", "nSets", "sets", "table", "subset")
  ret[[1]] <- c(xLab, yLab)
  ret[[2]] <- c(rows, cols)
  ret[[3]] <- nSets
  ret[[4]] <- data.frame(Set=1:nSets, Freq=0, Percent=0, Levels1=NA, Levels2=NA)
  ret[[5]] <- as.data.frame(tab)
  if (subset) {
    ret[[6]] <- data.frame(x=x, set=1)
    rownames(ret$subset) <- which(full)
  }
  ret$table <- ret$table[!(ret$table$Freq == 0), ]
  ret$table$set <- 1
  rownames(ret$table) <- 1:nrow(ret$table)

  for (i in seq(along=sets)) {
    tmp <- names(c)[c == sets[i]]
    tmp1 <- ret$table[ret$table$x %in% tmp, "y"]
    ret$sets[i, "Freq"] <- sum(x %in% tmp)
    ret$sets[i, "Percent"] <- ret$sets[i, "Freq"] / lengthX
    ret$sets[i, "Levels1"] <- paste(tmp, collapse=" ")
    ret$sets[i, "Levels2"] <- paste(tmp1, collapse=" ")
    ret$table[(ret$table$x %in% tmp), "set"] <- i
    if (subset) {
      ret$subset[(ret$subset$x %in% tmp), "set"] <- i
    }
  }
  if (sort) {
    ret$sets <- ret$sets[order(ret$sets$Freq, decreasing=TRUE), ]
    ret$sets[, "Set"] <- rownames(ret$sets) <- 1:nSets
  }

  class(ret) <- c("list", "connectedness")
  return(ret)
}

###------------------------------------------------------------------------
## connectedness.R ends here
