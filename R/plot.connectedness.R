## plot.connectedness.R
###------------------------------------------------------------------------
## What: Plot method
## $Id$
## Time-stamp: <2006-08-08 09:46:29 ggorjan>
###------------------------------------------------------------------------

plot.connectedness <- function(x, set=NULL, matrix=TRUE, scale=TRUE,
                               lines=FALSE, linesSet=NULL,
                               col=NULL,
                               plotArg=list(xlab=x$factors[1],
                                            ylab=x$factors[2]),
                               polygonArg=NULL,
                               pointsArg=list(pch=rep(19, x$nSets)),
                               linesArg=NULL, ...)
{

  ## --- Set handling ---

  ## Remove sets with unused levels
  tmp <- levelsBySet(x)
  tmp <- lapply(tmp, lapply, length)
  .fun <- function(x)
  {
    tmp <- lapply(x, "==", 0)
    if(any(tmp)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  tmp <- unlist(lapply(tmp, .fun))
  setMy <- x$sets$Set[!tmp]

  if (!is.null(set)) {
    if(!(set %in% setMy))
      stop("can not plot sets with unused levels")
  } else {
    set <- setMy
  }

  x$nSets <- length(set) ## Reduce number of sets
  x$table <- x$table[x$table$set %in% set, ] ## Keep only defined sets
  x$table$x <- factor(x$table$x) ## factorize values from a table
  x$table$y <- factor(x$table$y)
  x$nlevels <- c(nlevels(x$table$x), nlevels(x$table$y))
  x$table$set <- as.integer(as.factor(x$table$set)) ## Recode sets from 1:nSets

  ## --- Tick preparation ---

  if (scale) {
    xTick <- c(0, cumsum(prop.table(as.table(tapply(x$table$Freq,
                               list(x$table$x), sum, na.rm=TRUE))))) * x$nlevels[1]
    yTick <- c(0, cumsum(prop.table(as.table(tapply(x$table$Freq,
                               list(x$table$y), sum))))) * x$nlevels[2]
  } else {
    xTick <- 0:x$nlevels[1]
    yTick <- 0:x$nlevels[2]
  }

  atXTick <- atXTickDiff <- xTick[1:x$nlevels[1]]
  atYTick <- atYTickDiff <- yTick[1:x$nlevels[2]]
  for (i in 2:length(xTick)) {
    atXTickDiff[i - 1] <- (xTick[i] - xTick[i - 1]) / 2
    atXTick[i - 1] <- xTick[i] - atXTickDiff[i - 1]
  }
  for (i in 2:length(yTick)) {
    atYTickDiff[i - 1] <- (yTick[i] - yTick[i - 1]) / 2
    atYTick[i - 1] <- yTick[i] - atYTickDiff[i - 1]
  }

  xLev <- levels(x$table$x)
  yLev <- levels(x$table$y)
  x$table$atXTick <- atXTick[match(x$table$x, xLev)]
  x$table$atYTick <- atYTick[match(x$table$y, yLev)]

  ## --- Tweak default color and possibly missing col ---

  if (is.null(col)) {
    colN <- length(.colors)
    if (x$nSets > colN) {
      msg <- paste("Error: you have more than", colN, "sets;",
                   "you will have to specify colors by yourself")
      stop(msg)
    }
    col <- .colors[1:x$nSets]
  }
  if (is.null(polygonArg$col)) polygonArg$col <- col
  if (is.null(pointsArg$col)) pointsArg$col <- col
  if (is.null(linesArg$col)) linesArg$col <- col

  ## --- Argument recycling for polygon, points and lines ---

  recycle <- function(x, length)
  {
    tmp <- lapply(x, length)
    test <- tmp < length
    x[test] <- lapply(x[test],
                      function(xi) {
                        tmp <- vector(length=length)
                        tmp[1:length] <- xi
                        x <- tmp
                      })
    return(x)
  }

  if (matrix) {
    polygonArg <- recycle(x=polygonArg, length=x$nSets)
  } else {
    pointsArg <- recycle(x=pointsArg, length=x$nSets)
  }
  if (lines) linesArg <- recycle(x=linesArg, length=x$nSets)

  ## --- Plot ---

  plotArg <- c(list(x=x$table$atXTick, y=x$table$atYTick,
                    xlim=c(xTick[1], xTick[length(xTick)]),
                    ylim=c(yTick[1], yTick[length(yTick)]),
                    axes=FALSE, type="n"),
               plotArg)
  do.call("plot", args=plotArg)

  ## --- Grid ---

  abline(h=yTick)
  abline(v=xTick)

  ## --- Axes ---

  axis(1, at=atXTick, labels=xLev, tick=FALSE)
  axis(2, at=atYTick, labels=yLev, tick=FALSE)

  ## --- Matrix/points ---

  if (matrix) { ## Draw matrix with polygons
    x$table$xTick <- xTick[match(x$table$x, xLev)]
    x$table$yTick <- yTick[match(x$table$y, yLev)]
    x$table$atXTickDiff <- atXTickDiff[match(x$table$x, xLev)]
    x$table$atYTickDiff <- atYTickDiff[match(x$table$y, yLev)]

    for (i in 1:nrow(x$table)) {
      polygonArgTmp1 <- lapply(polygonArg, function(xi) xi[x$table[i, "set"]])
      polygonArgTmp <- c(list(x=c(x$table[i, "xTick"],
                                  x$table[i, "xTick"],
                                  x$table[i, "xTick"] + 2 * x$table[i, "atXTickDiff"],
                                  x$table[i, "xTick"] + 2 * x$table[i, "atXTickDiff"]),
                              y=c(x$table[i, "yTick"],
                                  x$table[i, "yTick"] + 2 * x$table[i, "atYTickDiff"],
                                  x$table[i, "yTick"] + 2 * x$table[i, "atYTickDiff"],
                                  x$table[i, "yTick"])),
                         polygonArgTmp1)
      do.call("polygon", args=polygonArgTmp)
    }
  } else { ## Draw points
    pointsArgTmp <- lapply(pointsArg, function(xi) xi[x$table[, "set"]])
    pointsArg <- c(list(x=x$table$atXTick, y=x$table$atYTick), pointsArgTmp)
    do.call("points", pointsArg)
  }

  ## --- Lines ---

  if (lines) {
    tmpList <- list(xLev=xLev, yLev=yLev, x=x$table$x, y=x$table$y)
    for (j in c(1, 2)) {
      for (i in seq(along=xLev)) {
        tmp <- x$table[tmpList[[j + 2]] == tmpList[[j]][i], c("set", "atXTick", "atYTick")]
        if (is.null(linesSet) | any(tmp$set %in% linesSet)) {
          linesArgTmp1 <- lapply(linesArg, function(xi) xi[tmp[1, "set"]])
          linesArgTmp <- c(list(x=tmp$atXTick, y=tmp$atYTick), linesArgTmp1)
          do.call("lines", linesArgTmp)
        }
      }
    }
  }

}

if (FALSE) { ## Code to generate qualitative colors
  if(require(RColorBrewer)) {
    pals <- c("Set1", "Dark2", "Accent", "Paired", "Set2", "Set3")
    palsN <- brewer.pal.info[pals, "maxcolors"]
    .colors <- vector(length=sum(palsN))
    j <- 1
    for (i in seq(along=pals)) {
      .colors[j:(j - 1 + palsN[i])] <- do.call("brewer.pal",
                                               args=list(n=palsN[i],
                                               name=pals[i]))
      j <- j + palsN[i]
    }
    .colors <- unique(.colors)
  }
}

.colors <-
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
    "#A65628", "#F781BF", "#999999", "#1B9E77", "#D95F02", "#7570B3",
    "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#7FC97F",
    "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17",
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
    "#FDBF6F", "#CAB2D6", "#6A3D9A", "#B15928", "#66C2A5", "#FC8D62",
    "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

###------------------------------------------------------------------------
## plot.connectedness.R ends here
