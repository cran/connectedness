### print.connectedness.R
###------------------------------------------------------------------------
### What: Print method
### $Id: print.connectedness.R 80 2007-03-04 11:52:04Z ggorjan $
### Time-stamp: <2007-03-03 18:44:55 ggorjan>
###------------------------------------------------------------------------

print.connectedness <- function(x, ...)
{
  cat("Connectedness between:", x$factors, "\n")
  cat("Number of disconnected subsets:", x$nSubsets, "\n")
  cat("Subsets:\n")

  ## --- Play with print width ---

  width <- getOption("width")

  ## Width of records an colnames
  charLen0 <- max(nchar(row.names(x$subsets))) + 1

  charLen1r <- max(nchar(x$subsets$Subset)) + 1 + max(nchar(x$subsets$Freq)) + 1 +
    max(nchar(format(x$subsets$Percent, ...))) + 1
  charLen1n <- charLen0 +
    nchar(paste(names(x$subsets[, c("Subset", "Freq", "Percent")]),
                collapse=" ")) + 1
  charLen1 <- max(charLen1r, charLen1n)

  charLen2c1 <- nchar(x$subsets$Levels1)
  charLen2c2 <- nchar(x$subsets$Levels2)
  charLen2r <- charLen2c1 + 1 + charLen2c2
  charLen2n <- nchar("Levels1 Levels2")
  charLen2 <- max(charLen2r, charLen2n)

  if(charLen0 + charLen1 + charLen2 > width) {
    testRow <- which(charLen2r + charLen1 + charLen0 > width)
    space <- floor((width - charLen0 - charLen1 - 1) / 2)
    tmp <- cbind(charLen2c1, charLen2c2)
    test <- tmp[testRow, ] > space
    space <- space - 3
    tmp <- x$subsets[testRow, c("Levels1", "Levels2")][test]
    x$subsets[testRow, c("Levels1", "Levels2")][test] <-
      paste(substr(tmp, start=1, stop=space), "...", sep="")
  }

  ## --- Print ---

  print(x$subsets, ...)
}

###------------------------------------------------------------------------
### print.connectedness.R ends here
