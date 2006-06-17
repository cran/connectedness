## runit.connectedness.R
###------------------------------------------------------------------------
## What: Tests for connectedness
## $Id$
## Time-stamp: <2006-08-08 09:51:58 ggorjan>
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

## levelBySet method

levelsBySetTest <- list()
levelsBySetTest$"1" <- list()
levelsBySetTest$"1"$Levels1 <- c("C", "D", "E", "G")
levelsBySetTest$"1"$Levels2 <- c("2", "4")
levelsBySetTest$"2" <- list()
levelsBySetTest$"2"$Levels1 <- c("A", "B", "F")
levelsBySetTest$"2"$Levels2 <- c("1", "3")

levelsBySetTestX1 <- levelsBySetTest
levelsBySetTestX1$"2" <- NULL
levelsBySetTestX1$"1"$Levels2 <- NULL

levelsBySetTestY12 <- levelsBySetTest
levelsBySetTestY12$"1"$Levels1 <- NULL
levelsBySetTestY12$"2"$Levels1 <- NULL

tmpD1 <- tmpD
class(tmpD1) <- "list"
### }}}
### {{{ --- connectedness ---

test.connectedness <- function()
{
  ## --- connectedness ---

  checkException(connectedness(x=1:14, y=1:5), silent=TRUE)
  ## other tests bellow test for the validity of output from connectedness()

  ## --- plot ---

  checkException(plot(x=tmp, set=3), silent=TRUE)

  ## --- subset ---

  checkException(subset(tmpSE), silent=TRUE)
  checkIdentical(subset(x=tmp, data=connect, set=1), sub1)
  checkIdentical(subset(x=tmp, data=connect, set=2), sub2)
  checkIdentical(subset(x=tmp, data=connect, set=c(1, 2)), sub12)
  checkEquals(subset(x=tmp, data=connect), connect) ## why does identical fail here?
  checkIdentical(subset(x=tmp, data=connect, set=1, dropNA=TRUE), subD1)

  ## --- levelsBySet ---

  checkIdentical(levelsBySet(x=tmpD), levelsBySetTest)
  checkIdentical(levelsBySet(x=tmpD, set=1, factor="x"), levelsBySetTestX1)
  checkIdentical(levelsBySet(x=tmpD, set=c(1, 2), factor="y"), levelsBySetTestY12)
  checkException(levelsBySet(tmpD1), silent=TRUE)
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
## runit.connectedness.R ends here
