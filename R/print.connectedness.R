## print.connectedness.R
###------------------------------------------------------------------------
## What: Print method
## $Id$
## Time-stamp: <2006-08-08 09:49:47 ggorjan>
###------------------------------------------------------------------------

print.connectedness <- function(x, ...)
{
  cat("Connectedness between:", x$factors, "\n")
  cat("Number of disconnected sets:", x$nSets, "\n")
  cat("Sets:\n")

  ## --- Play with print width ---

  width <- getOption("width")

  ## Width of records an colnames
  charLen0 <- max(nchar(rownames(x$sets))) + 1

  charLen1r <- max(nchar(x$sets$Set)) + 1 + max(nchar(x$sets$Freq)) + 1 +
    max(nchar(format(x$sets$Percent, ...))) + 1
  charLen1n <- charLen0 +
    nchar(paste(colnames(x$sets[, c("Set", "Freq", "Percent")]),
                collapse=" ")) + 1
  charLen1 <- max(charLen1r, charLen1n)

  charLen2c1 <- nchar(x$sets$Levels1)
  charLen2c2 <- nchar(x$sets$Levels2)
  charLen2r <- charLen2c1 + 1 + charLen2c2
  charLen2n <- nchar("Levels1 Levels2")
  charLen2 <- max(charLen2r, charLen2n)

  if (charLen0 + charLen1 + charLen2 > width) {
    testRow <- which(charLen2r + charLen1 + charLen0 > width)
    space <- floor((width - charLen0 - charLen1 - 1) / 2)
    tmp <- cbind(charLen2c1, charLen2c2)
    test <- tmp[testRow, ] > space
    space <- space - 3
    tmp <- x$sets[testRow, c("Levels1", "Levels2")][test]
    x$sets[testRow, c("Levels1", "Levels2")][test] <-
      paste(substr(tmp, start=1, stop=space), "...", sep="")
  }

  ## --- Print ---

  print(x$sets, ...)
}

###------------------------------------------------------------------------
## print.connectedness.R ends here
