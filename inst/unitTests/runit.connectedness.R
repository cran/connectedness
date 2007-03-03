### runit.connectedness.R
###------------------------------------------------------------------------
### What: Tests for connectedness
### $Id: runit.connectedness.R 80 2007-03-04 11:52:04Z ggorjan $
### Time-stamp: <2007-03-03 21:10:15 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("connectedness")
}

data(connect)
tmp <- connectedness(x=connect$group, y=connect$season)
tmpSE <- connectedness(x=connect$group, y=connect$season, subset=FALSE)
tmpD <- connectedness(x=connect$group, y=connect$season, drop=TRUE)

## Subset method

sub1 <- connect[c(2, 3, 4, 5, 10, 14), ]
sub2 <- connect[c(1, 6, 7, 8, 9, 11, 13), ]
sub12 <- rbind(sub1, sub2)
sub12 <- sub12[order(as.integer(rownames(sub12))), ]

subD1 <- connect[c(2, 3, 4, 10), ]

## levelBySubset method

levelsBySubsetTest <- list()
levelsBySubsetTest$"2" <- list()
levelsBySubsetTest$"2"$Levels1 <- c("C", "D", "E", "G")
levelsBySubsetTest$"2"$Levels2 <- c("2", "4")
levelsBySubsetTest$"1" <- list()
levelsBySubsetTest$"1"$Levels1 <- c("A", "B", "F")
levelsBySubsetTest$"1"$Levels2 <- c("1", "3")

levelsBySubsetTestX1 <- levelsBySubsetTest
levelsBySubsetTestX1$"2" <- NULL
levelsBySubsetTestX1$"1"$Levels2 <- NULL

levelsBySubsetTestY12 <- levelsBySubsetTest
levelsBySubsetTestY12$"1"$Levels1 <- NULL
levelsBySubsetTestY12$"2"$Levels1 <- NULL

tmpD1 <- tmpD
class(tmpD1) <- "list"

### }}}
### {{{ --- connectedness ---

test.connectedness <- function()
{
  ## --- connectedness ---

  checkException(connectedness(x=1:14, y=1:5))
  ## other tests bellow test for the validity of output from connectedness()

  ## --- plot ---

  checkException(plot(x=tmp, subset=3))

  ## --- subset ---

  checkException(subset(tmpSE))
  checkIdentical(subset(x=tmp, data=connect, subset=1), sub1)
  checkIdentical(subset(x=tmp, data=connect, subset=2), sub2)
  checkIdentical(subset(x=tmp, data=connect, subset=c(1, 2)), sub12)
  checkEquals(subset(x=tmp, data=connect), connect) ## why does identical fail here?
  checkIdentical(subset(x=tmp, data=connect, subset=1, dropNA=TRUE), subD1)

  ## --- levelsBySubset ---

  checkIdentical(levelsBySubset(x=tmpD), levelsBySubsetTest)
  checkIdentical(levelsBySubset(x=tmpD, subset=1, factor="x"), levelsBySubsetTestX1)
  checkIdentical(levelsBySubset(x=tmpD, subset=c(1, 2), factor="y"), levelsBySubsetTestY12)
  checkException(levelsBySubset(tmpD1))
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.connectedness.R ends here
