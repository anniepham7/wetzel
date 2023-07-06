\name{coradarBoxplot}
\alias{coradarBoxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RadarBoxplot
}
\description{
The radar-boxplot function presented here is a modification of the radarchart function from the library fsmb. This modification introduces the concept of the radar-boxplot, which combines elements of both radar charts and boxplots into a single visualization.

The original radarchart function provided by the fsmb library is a useful tool for creating radar charts. However, to enhance its capabilities and enable the visualization of distributional characteristics, this function has been modified to incorporate boxplot elements.

By merging the radar chart and boxplot concepts, the radar-boxplot function allows for the simultaneous display of multiple variables while providing insights into their distributions. This modified function provides a more comprehensive understanding of the data by representing the median and quartilers of each variable in a radar-like structure.

The difference between coradarBoxplot (The loop iteration for drawing the polygons and axis labels starts at 1.) and wradarBoxplot (The loop iteration for drawing the polygons and axis labels starts at 0.) functions is primarily in the way they handle the axis labels and scaling of the plot.
This distinction affects the positioning of polygons, axis labels, and other elements in the plot. The choice of starting index in each function may have been made based on specific design considerations or requirements.
}
\usage{
coradarBoxplot(df, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, plwd = 1, pdensity = 0.01, pangle = 45, pfcol = NA, cglty = 3, cglwd = 1, cglcol = "darkgrey", axislabcol = "black", title = "", maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, palcex = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
The data frame to be used to draw radarchart. If maxmin is TRUE, this must include maximum values as row 1 and minimum values as row 2 for each variables, and actual data should be given as row 3 and lower rows. The number of columns (variables) must be more than 2.
}
  \item{seg}{

The number of segments for each axis (default 4).
}
  \item{pty}{
A vector to specify point symbol.
}
  \item{pcol}{
A vector of color codes for plot data
}
  \item{plty}{
A vector of line types for plot data
}
  \item{plwd}{
%%     ~~Describe \code{plwd} here~~
}
  \item{pdensity}{
A vector of filling density of polygons: Default NULL, which is repeatedly used.
}
  \item{pangle}{
A vector of the angles of lines used as filling polygons: Default 45, which is repeatedly used.
}
  \item{pfcol}{
A vector of color codes for filling polygons: Default NA, which is repeatedly usd.
}
  \item{cglty}{
Line type for radar grids: Default 3.
}
  \item{cglwd}{
Line width for radar grids: Default 1.
}
  \item{cglcol}{
Line color for radar grids: Default "darkgrey"
}
  \item{axislabcol}{
Color of axis label and numbers: Default "black"
}
  \item{title}{
Name of the title
}
  \item{maxmin}{
Logical. If true, data frame includes possible maximum values as row 1 and possible minimum values as row 2. If false, the maximum and minimum values for each axis will be calculated as actual maximum and minimum of the data. Default TRUE.
}
  \item{na.itp}{
Logical. If true, items with NA values are interpolated from nearest neighbor items and connect them. If false, items with NA are treated as the origin (but not pointed, only connected with lines). Default FALSE.
}
  \item{centerzero}{
Logical. If true, this function draws charts with scaling originated from (0,0). If false, charts originated from (1/segments). Default FALSE.
}
  \item{vlabels}{
Character vector for the names for variables. If NULL, the names of the variables as colnames(df) are used. Default NULL.
}
  \item{vlcex}{
Font size magnification for vlabels. If NULL, the font size is fixed at text()'s default. Default NULL.
}
  \item{caxislabels}{
Character vector for center axis labels, overwriting values specified in axistype option. If NULL, the values specified by axistype option are used. Default is NULL.
}
  \item{calcex}{
Font size magnification for caxislabels. If NULL, the font size is fixed at text()'s default. Default NULL.
}
  \item{paxislabels}{
Character vector for around-the-chart (peripheral) labels, overwriting values specified in axistype option. If NULL, the values specified by axistype option are used. Default is NULL.
}
  \item{palcex}{
Font size magnification for paxislabels. If NULL, the font size is fixed at text()'s default. Default NULL.
}
  \item{\dots}{
Miscellaneous arguments to be given for plot.default().
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://www.rdocumentation.org/packages/fmsb/versions/0.7.5/topics/radarchart
}
\author{
%%  ~~who you are~~
}
\note{
The original radarchart function provided by Minato Nakazawa minatonakazawa@gmail.com https://minato.sip21c.org/
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
https://www.rdocumentation.org/packages/fmsb/versions/0.7.5/topics/radarchart
}
\examples{
library(wetzel)
\dontshow{coradarBoxplot <- function (df, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
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
    for (i in 0:seg) {
        polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg +
            CGap), lty = cglty, lwd = cglwd, border = cglcol)
        if (axistype == 1 | axistype == 3)
            CAXISLABELS <- paste(i/seg * 100, "(\%)")
        if (axistype == 4 | axistype == 5)
            CAXISLABELS <- sprintf("\%3.2f", i/seg)
        if (axistype == 2)
            CAXISLABELS <- paste(i)
        if (!is.null(caxislabels) & (i < length(caxislabels)))
            CAXISLABELS <- caxislabels[i + 1]
        if (axistype == 1 | axistype == 3 | axistype == 4 | axistype ==
            5 | axistype == 2) {
            if (is.null(calcex))
                text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
                  col = axislabcol)
            else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
                col = axislabcol, cex = calcex)
        }
    }
    if (centerzero) {
        arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty,
            length = 0, col = cglcol)
    }
    else {
        arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy *
            1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
    }
    PAXISLABELS <- df[1, 1:n]
    if (!is.null(paxislabels))
        PAXISLABELS <- paxislabels
    if (axistype == 3 | axistype == 5) {
        if (is.null(palcex))
            text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
        else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol,
            cex = palcex)
    }
    VLABELS <- colnames(df)
    if (!is.null(vlabels))
        VLABELS <- vlabels
    if (is.null(vlcex))
        text(xx * 1.2, yy * 1.2, VLABELS)
    else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex)
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
        scaleLower <- CGap/(seg + CGap) + (lowerQuartile - df[2,
            ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
        scaleUpper <- CGap/(seg + CGap) + (upperQuartile - df[2,
            ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
        xLower <- c(xx * scaleLower, xx[1] * scaleLower[1])
        yLower <- c(yy * scaleLower, yy[1] * scaleLower[1])
        xUpper <- c(xx * scaleUpper, xx[1] * scaleUpper[1])
        yUpper <- c(yy * scaleUpper, yy[1] * scaleUpper[1])
        polygon(c(xLower, rev(xUpper)), c(yLower, rev(yUpper)),
            col = rgb(0.5, 0.5, 0.5, alpha = pdensity), border = NA)
        medians <- apply(df[-c(1, 2), ], 2, median, na.rm = TRUE)
        median_scale <- CGap/(seg + CGap) + (medians - df[2,
            ])/(df[1, ] - df[2, ]) * seg/(seg + CGap)
        xx_medians <- xx * median_scale
        yy_medians <- yy * median_scale
        points(xx_medians, yy_medians, pch = 16, col = "red")
        lines(c(xx_medians, xx_medians[1]), c(yy_medians, yy_medians[1]),
            col = "red", lwd = 2)
    }
  }}
# Demo data
dataset <- data.frame(
  Var1 = runif(100, 0, 7),
  Var2 = runif(100, 0, 7),
  Var3 = runif(100, 0, 7),
  Var4 = runif(100, 0, 7),
  Var5 = runif(100, 0, 7),
  Var6 = runif(100, 0, 7)
)

# Data must be given as the data frame, where the first cases show maximum.
max_min <- data.frame(
  Var1 = c(7, 0), Var2 = c(7, 0), Var3 = c(7, 0),
  Var4 = c(7,0), Var5 = c(7, 0), Var6 = c(7,0)
)

rownames(max_min) <- c("Max", "Min")
df <- rbind(max_min, dataset)
coradarBoxplot(df, seg = 7, title = "Variables", centerzero =TRUE, vlabels = c("Var1", "Var2", "Var3","Var4","Var5","Var6"), cglcol = "black", maxmin = TRUE, pdensity = 0.006, calcex = 0.7, vlcex = 0.75)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ ~radarplot }
\keyword{ ~boxplot }
\keyword{ ~spiderplot }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
