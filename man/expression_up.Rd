\name{expression_up}
\alias{expression_up}

\title{
Identify and sort genes overexpressed in one condition agains another in an affymetrix dataset
}
\description{
For each probeset of the dataset, the algorithm calculates the maximum level of expression in control samples (M) and select all test samples with and expression 
exceedings M + a threshold. A score is then attributed to the gene on the basis of the subpopulation selected size and overexpression.
}
\usage{
expression_up(eset, TUM, CTRL, seuil)
}

\arguments{
  \item{eset}{

An affymetrix expressionset
}
  \item{TUM}{
index of the test samples in the dataset
}
  \item{CTRL}{
index of the control samples in the dataset
}
  \item{seuil}{
threshold to use
Depending of the type of control, we usually choose 0.5 for an homogene control population and 0.2 for an heterogene control population
}
}

\value{

An affymetrix expressionSet containing results of the algorithm for every probesets : score of overexpression, number of samples in which the gene is overexpressed, 
maximum expression in control samples, delta (difference between the median of the subpopulation selected and the maximum of control samples),
statistics about the population overexpressed (mean, median, standard deviation, interquartile range), names of samples in this subpopulation.
This expressionSet in ranked by score in probesets.
}

\author{
Noemie Robil
}





\examples{


library(Biobase)
library(affy)
data(sample.ExpressionSet)
TEST=which(pData(sample.ExpressionSet)[,"type"]=="Case")
CTRL=which(pData(sample.ExpressionSet)[,"type"]=="Control")
data_up=expression_up(sample.ExpressionSet,TEST,CTRL,0.5)


## The function is currently defined as
function (eset, TUM, CTRL, seuil) 
{
    newData <- t(apply(exprs(eset), 1, function(v) {
        lc <- length(CTRL)
        lt <- length(TUM)
        limit = floor(lc * 0.025) + 1
        CTRL2 = CTRL
        M <- max(v[CTRL2])
        t = 0
        ind_M <- which.max(v[CTRL2])
        seuil_M <- quantile(v[CTRL2], 3/4) + 3 * IQR(v[CTRL2])
        i = 1
        while (M > seuil_M && i <= limit) {
            t = t + 1
            CTRL2 = CTRL2[-ind_M]
            M = max(v[CTRL2])
            ind_M = which.max(v[CTRL2])
            i = i + 1
        }
        pop <- colnames(exprs(eset)[, TUM][, which(v[TUM] > seuil + 
            M)])
        l <- length(pop)
        if (l > 0) 
            Delta_median_up <- median(v[pop]) - M
        else Delta_median_up <- 0
        score_up <- 2^(Delta_median_up) * l/lt
        return(c(score_up, l, M, t, Delta_median_up, mean(v[pop]), 
            median(v[pop]), sd(v[pop]), IQR(v[pop]), paste(pop, 
                collapse = ",")))
    }))
    colnames(newData) <- c("Score_up", "Numbre_up", "Max_normal", 
        "Outliers", "Delta_median_up", "Mean_up", "Median_up", 
        "sd_up", "IQR_up", "Samples_up")
    pData(featureData(eset)) <- cbind(pData(featureData(eset)), 
        newData)
    eset = eset[order(as.numeric(as.character(featureData(eset)$Score_up)), 
        decreasing = TRUE), ]
    return(eset)
  }
}

