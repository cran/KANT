\name{plot_up}
\alias{plot_up}

\title{
Function to plot the results of expression_up
}
\description{
Generates a graphic of the 50 best probesets of the algorithm. Every probeset is  plotted in function of the number of samples in the subpopulation 
that overexpressed it and delta (difference of expression between max of normal samples and median of the subpopulation). They are represented by a circle, with a
size proportional to  1/(standard deviation of expression in the subpopulation)
}
\usage{
plot_up(N, eset_up, cells_type, fontsize)
}

\arguments{
  \item{N}{
Number of probesets to plot
}
  \item{eset_up}{
Result of expression_up
}
  \item{cells_type}{
Name of the type of cells tested (just for title of the graph)
}
  \item{fontsize}{
For legends
}
}

\author{
Noemie Robil
}




\seealso{

See Also \code{link{expression_up}}
}
\examples{


## The function is currently defined as
function (N, eset_up, cells_type, fontsize) 
{
    probes <- 1:N
    plot(as.numeric(as.character(pData(featureData(eset_up))[probes, 
        "Numbre_up"])), as.numeric(as.character(pData(featureData(eset_up))[probes, 
        "Delta_median_up"])), 
        cex = (1/(0.1 + as.numeric(as.character(pData(featureData(eset_up))[probes, 
        "IQR_up"])))), pch = 21, main = c("Over-expression in  :", 
        cells_type), xlab = "Number of samples in the sub-population over-expressing the gene", 
        ylab = "Delta")
    text(as.numeric(as.character(pData(featureData(eset_up))[probes, 
        "Numbre_up"])), as.numeric(as.character(pData(featureData(eset_up))[probes, 
        "Delta_median_up"])), as.character(pData(featureData(eset_up))[probes, 
        "Gene.Symbol"]), pos = 2, cex = fontsize)
  }
}

