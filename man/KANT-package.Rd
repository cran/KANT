\name{KANT-package}
\alias{KANT-package}
\alias{KANT}
\docType{package}
\title{
KANT : Kancer Antigene Tracker
}
\description{
This algorithm was designed to identify and sort genes, associated to transmembrane protein, found to be more strongly expressed in a tumor dataset than in a normal dataset.
It can be used in other conditions. 

}
\details{
\tabular{ll}{
Package: \tab KANT\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-12\cr
License: \tab GPL\cr
}
step 1 : 
Selection of genes associated to transmembrane proteins in an expressionSet affymetrix using data(TM), that contains geneID of theses genes.
step 2 :  
TEST=which(pData(ExpressionSet)[,"type"]=="Case")
CTRL=which(pData(ExpressionSet)[,"type"]=="Control")
data_up=expression_up(sample.ExpressionSet,TEST,CTRL,0.5)
plot_up(50,data_up, "Test",0.8)
}
\author{
Noemie Robil

Maintainer: Who to complain to <noemie.robil@ligue-cancer.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }

