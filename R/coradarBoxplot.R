#' radarBoxplot
#'
#' The radar-boxplot function presented here is a modification of the radarchart function from the library `fsmb`. This modification introduces the concept of the radar-boxplot, which combines elements of both radar charts and boxplots into a single visualization. The original radarchart function provided by the fsmb library is a useful tool for creating radar charts. However, to enhance its capabilities and enable the visualization of distributional characteristics, this function has been modified to incorporate boxplot elements. By merging the radar chart and boxplot concepts, the radar-boxplot function allows for the simultaneous display of multiple variables while providing insights into their distributions. This modified function provides a more comprehensive understanding of the data by representing the median and quartilers of each variable in a radar-like structure.
#'
#' @param df A dataframe containing the variables to be plotted.
#' @param seg The number of concentric circles or segments in the plot. Default is 4.
#' @param pty The type of points to be plotted. Default is 16.
#' @param pcol The color(s) of the points. Default is 1:8.
#' @param plty The type(s) of line(s) to be plotted. Default is 1:6.
#' @param plwd The width(s) of the lines. Default is 1.
#' @param pdensity The density of shading between lower and upper quartiles. Default is 0.01.
#' @param pangle The angle(s) of shading lines. Default is 45.
#' @param pfcol The fill color(s) of the polygons. Default is NA.
#' @param cglty The line type of the concentric circles. Default is 3.
#' @param cglwd The line width of the concentric circles. Default is 1.
#' @param cglcol The color of the concentric circles. Default is "darkgrey".
#' @param axislabcol The color of the axis labels. Default is "black".
#' @param title The title of the radar-boxplot. Default is an empty string.
#' @param maxmin Logical value indicating whether to include maximum and minimum values in the plot. Default is TRUE.
#' @param na.itp Logical value indicating whether to interpolate missing values. Default is TRUE.
#' @param centerzero Logical value indicating whether to center the zero value on the axis. Default is FALSE.
#' @param vlabels Custom labels for the variables. Default is NULL.
#' @param vlcex The cex value for the variable labels. Default is NULL.
#' @param caxislabels Custom axis labels. Default is NULL.
#' @param calcex The cex value for the concentric circle axis labels. Default is NULL.
#' @param paxislabels Custom labels for the variable axis. Default is NULL.
#' @param palcex The cex value for the variable axis labels. Default is NULL.
#' @param ... Additional arguments to be passed to the plot function.
#' @importFrom grDevices rgb
#' @importFrom stats median quantile
#' @importFrom graphics arrows lines points polygon text
#' @seealso
#' The original radarchart function provided by Minato Nakazawa minatonakazawa[at]gmail.com, \link{https://minato.sip21c.org/} and \link{https://www.rdocumentation.org/packages/fmsb/versions/0.7.5/topics/radarchart}
#' @examples
#' \dontshow{coradarBoxplot <- function (df, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
#' plwd = 1, pdensity = 0.01, pangle = 45, pfcol = NA, cglty = 3,
#' cglwd = 1, cglcol = "darkgrey", axislabcol = "black", title = "",
#' maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL,
#' vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL,
#' palcex = NULL, ...)
#' {
#'   if (!is.data.frame(df)) {
#'     cat("The data must be given as dataframe.\n")
#'     return()
#'   }
#'   if ((n <- length(df)) < 3) {
#'     cat("The number of variables must be 3 or more.\n")
#'     return()
#'   }
#'   if (maxmin == FALSE) {
#'     dfmax <- apply(df, 2, max)
#'     dfmin <- apply(df, 2, min)
#'     df <- rbind(dfmax, dfmin, df)
#'   }
#'   plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE,
#'        axes = FALSE, xlab = "", ylab = "", main = title, asp = 1,
#'        ...)
#'   theta <- seq(90, 450, length = n + 1) * pi/180
#'   theta <- theta[1:n]
#'   xx <- cos(theta)
#'   yy <- sin(theta)
#'   CGap <- ifelse(centerzero, 0, 1)
#'   #axistype <- 2
#'   for (i in 0:(seg-1)) {
#'     polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + CGap),
#'             lty = cglty, lwd = cglwd, border = cglcol)
#'     CAXISLABELS <- paste(i+1)
#'     if (!is.null(caxislabels) & (i < length(caxislabels)))
#'       CAXISLABELS <- caxislabels[i + 1]
#'     if (is.null(calcex))
#'       text(-0.025, (i + CGap)/(seg + CGap), CAXISLABELS, col = axislabcol)
#'     else
#'       text(-0.025, (i + CGap)/(seg + CGap), CAXISLABELS, col = axislabcol, cex = calcex)
#'   }
#'   if (centerzero) {
#'     arrows(0, 0, xx * (seg-1) / seg, yy * (seg-1) / seg * (seg / (seg + CGap)), lwd = cglwd, lty = cglty,
#'            length = 0, col = cglcol)
#'   } else {
#'     arrows(xx/(seg + CGap), yy/(seg + CGap), xx * (seg-1) / seg, yy * (seg-1) / seg,
#'            lwd = cglwd, lty = cglty, length = 0, col = cglcol)
#'   }
#'
#'   PAXISLABELS <- df[1, 1:n]
#'   if (!is.null(paxislabels))
#'    PAXISLABELS <- paxislabels
#'   VLABELS <- colnames(df)
#'   if (!is.null(vlabels))
#'     VLABELS <- vlabels
#'   if (is.null(vlcex))
#'     text(xx * 1, yy * 1, VLABELS)
#'   else text(xx * 1, yy * 1, VLABELS, cex = vlcex)
#'   series <- length(df[[1]])
#'   SX <- series - 2
#'   if (length(pty) < SX) {
#'     ptys <- rep(pty, SX)
#'   }
#'   else {
#'     ptys <- pty
#'   }
#'   if (length(pcol) < SX) {
#'     pcols <- rep(pcol, SX)
#'  }
#'   else {
#'     pcols <- pcol
#'   }
#'   if (length(plty) < SX) {
#'     pltys <- rep(plty, SX)
#'   }
#'   else {
#'     pltys <- plty
#'   }
#'   if (length(plwd) < SX) {
#'     plwds <- rep(plwd, SX)
#'   }
#'   else {
#'     plwds <- plwd
#'   }
#'   if (length(pdensity) < SX) {
#'     pdensities <- rep(pdensity, SX)
#'   }
#'   else {
#'     pdensities <- pdensity
#'   }
#'  if (length(pangle) < SX) {
#'    pangles <- rep(pangle, SX)
#'  }
#'  else {
#'     pangles <- pangle
#'   }
#'   if (length(pfcol) < SX) {
#'     pfcols <- rep(pfcol, SX)
#'   }
#'   else {
#'     pfcols <- pfcol
#'   }
#'   medians <- apply(df, 2, median, na.rm = TRUE)
#'   lowerQuartile <- apply(df, 2, function(x) quantile(x, 0.25))
#'   upperQuartile <- apply(df, 2, function(x) quantile(x, 0.75))
#'   for (i in 3:series) {
#'     xxs <- xx
#'     yys <- yy
#'
#'     # Berechnung der Skalierung für das untere (Q1) und obere (Q3) Quartil
#'     # Berechnung der Skalierung für das untere (Q1) und obere (Q3) Quartil
#'     scaleLower <- (CGap-1)/(seg + CGap) + (lowerQuartile - df[2, ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
#'     scaleUpper <- (CGap-1)/(seg + CGap) + (upperQuartile - df[2, ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
#'
#'
#'     # Berechnung der Koordinaten für das untere (Q1) und obere (Q3) Quartil
#'    xLower <- c(xx * scaleLower, xx[1] * scaleLower[1])
#'     yLower <- c(yy * scaleLower, yy[1] * scaleLower[1])
#'     xUpper <- c(xx * scaleUpper, xx[1] * scaleUpper[1])
#'     yUpper <- c(yy * scaleUpper, yy[1] * scaleUpper[1])
#'
#'    # Zeichnen eines Polygons (Schattierung) zwischen dem unteren (Q1) und oberen (Q3) Quartil
#'     polygon(c(xLower, rev(xUpper)), c(yLower, rev(yUpper)), col=rgb(0.5, 0.5, 0.5, alpha = pdensity) , border = NA)
#'
#'     # Mediane für jede Spalte berechnen
#'    medians <- apply(df[-c(1, 2), ], 2, median, na.rm = TRUE)
#'
#'     # Skalierung für Mediane berechnen
#'     median_scale <- (CGap-1) / (seg + CGap) + (medians - df[2, ]) / (df[1, ] - df[2, ]) * seg / (seg + CGap)
#'
#'    # Koordinaten für Mediane berechnen
#'     xx_medians <- xx * median_scale
#'     yy_medians <- yy * median_scale
#'
#'     # Mediane Punkte plotten
#'     points(xx_medians, yy_medians, pch = 16, col = "red")
#'
#'     # Mediane Punkte zu einem Ring verbinden
#'     lines(c(xx_medians, xx_medians[1]), c(yy_medians, yy_medians[1]), col = "red", lwd = 2)
#'
#'   }
#' }
#' dataset <- data.frame(Var1 = runif(100, 0, 7),Var2 = runif(100, 0, 7),Var3 = runif(100, 0, 7),Var4 = runif(100, 0, 7),Var5 = runif(100, 0, 7),Var6 = runif(100, 0, 7))
#' max_min <- data.frame( Var1 = c(7, 0), Var2 = c(7, 0), Var3 = c(7, 0), Var4 = c(7,0), Var5 = c(7, 0), Var6 = c(7,0))
#' rownames(max_min) <- c("Max", "Min")
#' df <- rbind(max_min, dataset)
#' coradarBoxplot(df, seg = 7, title = "Variables", centerzero = TRUE,
#' vlabels = c("Var1", "Var2", "Var3","Var4","Var5","Var6"), cglcol = "black",
#' maxmin = TRUE, pdensity = 0.006, calcex = 0.7, vlcex = 0.75)
#' @export
coradarBoxplot <- function (df, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
                            plwd = 1, pdensity = 0.01, pangle = 45, pfcol = NA, cglty = 3,
                            cglwd = 1, cglcol = "darkgrey", axislabcol = "black", title = "",
                            maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL,
                            vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL,
                            palcex = NULL, ...)
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE,
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1,
       ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  #axistype <- 2
  for (i in 0:(seg-1)) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + CGap),
            lty = cglty, lwd = cglwd, border = cglcol)
    CAXISLABELS <- paste(i+1)
    if (!is.null(caxislabels) & (i < length(caxislabels)))
      CAXISLABELS <- caxislabels[i + 1]
    if (is.null(calcex))
      text(-0.025, (i + CGap)/(seg + CGap), CAXISLABELS, col = axislabcol)
    else
      text(-0.025, (i + CGap)/(seg + CGap), CAXISLABELS, col = axislabcol, cex = calcex)
  }
  if (centerzero) {
    arrows(0, 0, xx * (seg-1) / seg, yy * (seg-1) / seg * (seg / (seg + CGap)), lwd = cglwd, lty = cglty,
           length = 0, col = cglcol)
  } else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * (seg-1) / seg, yy * (seg-1) / seg,
           lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }

  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels))
    PAXISLABELS <- paxislabels
  VLABELS <- colnames(df)
  if (!is.null(vlabels))
    VLABELS <- vlabels
  if (is.null(vlcex))
    text(xx * 1, yy * 1, VLABELS)
  else text(xx * 1, yy * 1, VLABELS, cex = vlcex)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  medians <- apply(df, 2, median, na.rm = TRUE)
  lowerQuartile <- apply(df, 2, function(x) quantile(x, 0.25))
  upperQuartile <- apply(df, 2, function(x) quantile(x, 0.75))
  for (i in 3:series) {
    xxs <- xx
    yys <- yy

    # Berechnung der Skalierung für das untere (Q1) und obere (Q3) Quartil
    # Berechnung der Skalierung für das untere (Q1) und obere (Q3) Quartil
    scaleLower <- (CGap-1)/(seg + CGap) + (lowerQuartile - df[2, ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
    scaleUpper <- (CGap-1)/(seg + CGap) + (upperQuartile - df[2, ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)


    # Berechnung der Koordinaten für das untere (Q1) und obere (Q3) Quartil
    xLower <- c(xx * scaleLower, xx[1] * scaleLower[1])
    yLower <- c(yy * scaleLower, yy[1] * scaleLower[1])
    xUpper <- c(xx * scaleUpper, xx[1] * scaleUpper[1])
    yUpper <- c(yy * scaleUpper, yy[1] * scaleUpper[1])

    # Zeichnen eines Polygons (Schattierung) zwischen dem unteren (Q1) und oberen (Q3) Quartil
    polygon(c(xLower, rev(xUpper)), c(yLower, rev(yUpper)), col=rgb(0.5, 0.5, 0.5, alpha = pdensity) , border = NA)

    # Mediane für jede Spalte berechnen
    medians <- apply(df[-c(1, 2), ], 2, median, na.rm = TRUE)

    # Skalierung für Mediane berechnen
    median_scale <- (CGap-1) / (seg + CGap) + (medians - df[2, ]) / (df[1, ] - df[2, ]) * seg / (seg + CGap)

    # Koordinaten für Mediane berechnen
    xx_medians <- xx * median_scale
    yy_medians <- yy * median_scale

    # Mediane Punkte plotten
    points(xx_medians, yy_medians, pch = 16, col = "red")

    # Mediane Punkte zu einem Ring verbinden
    lines(c(xx_medians, xx_medians[1]), c(yy_medians, yy_medians[1]), col = "red", lwd = 2)

  }
}

