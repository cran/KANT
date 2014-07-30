\name{expression_up}
\alias{expression_up}

\title{
Identify and sort genes overexpressed in one condition agains another in an affymetrix dataset or a matrix of data (genes per row, samples per column)
}
\description{
For each probeset of the dataset, the algorithm calculates the maximum level of expression in control samples (M) and select all test samples with and expression 
exceedings M + a threshold. A score is then attributed to the gene on the basis of the subpopulation selected size and overexpression.
}
\usage{
expression_up(data,type="eset", CASE, CTRL,seuil=0.5)
}

\arguments{
  \item{data}{

An affymetrix expressionset 
}
  \item{type}{
  Type of the data : "eset" if it is an affymetrix expressionSet with featureData; "tab" it it is a matrix of data (genes per row,samples per column)
}
  \item{CASE}{
index of the case samples in the dataset
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

An affymetrix expressionSet (or a matrix) containing results of the algorithm for every probesets : score of overexpression, number of samples in which the gene is overexpressed, 
maximum expression in control samples, delta (difference between the median of the subpopulation selected and the maximum of control samples),
statistics about the population overexpressed (mean, median, standard deviation, interquartile range), names of samples in this subpopulation.
This expressionSet (or matrix) in ranked by score in probesets.
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
data_up=expression_up(sample.ExpressionSet,type="eset",TEST,CTRL,0.5)
}


