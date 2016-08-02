\name{SXTpls}
\alias{SXTpls}
\alias{SXTpls}
\docType{package}
\title{
SXTpls
}
\description{
PLS model
}

\usage{
SXTpls(sample=NULL,qc=NULL,info=NULL,
                 #used data
      scalemethod="auto", plsmethod=c("plsreg","plsr"),
      width=7,height=7,QC=FALSE,text=FALSE,ellipse=FALSE,
      color=c("palegreen","firebrick1","royalblue","yellow","black","cyan","gray48"),
      shape=c(17,19,15,18,2,8,11),cexa=1)
}

\arguments{
\tabular{ll}{
sample: \tab a matrix whose column reorents peak and row represents sample\cr
qc: \tab a matrix whose column reorents peak and row represents qc\cr
info: \tab a matrix whose colum represent the classes of samples\cr
scalemethod: \tab the scale method of sample, default is pareto\cr
plsmethod: \tab which pls function you want to use, plsreg or plsr\cr
width&heigh: \tab the width and heigh of plot, default are 7\cr
QC: \tab the qc samples are drawn in the plot or not, default is FALSE\cr
text: \tab the samples' names are writen in the plot or not, default is FALSE\cr
ellipse: \tab the ellipse is drawn in the plot or not, default is FALSE\cr
color: \tab the colors for the different classes of samples,\cr
       \tab default are c("green","red","blue","yellow","black","cyan","gray48")\cr
shape: \tab the shapes for the different classes of samples, default are c(17,19,15,18,2,8,11)\cr
cexa: \tab the size of the points in PLS score plot\cr
}
}

\details{
\tabular{ll}{
Package: \tab SXTpls\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Note for version 1.1: \tab Add the Q2cum, R2Xcum and R2Ycum barplot and output it.(2015-11-04)\cr
Date: \tab 2015-06-05# this is a good day for me\cr
License: \tab No\cr

}
}

\value{
\tabular{ll}{
plsplot 2d: \tab PLS score plot 2 D\cr
plsplot 3d: \tab PLS score plot 3 D\cr
Q2cum plot: \tab barplot for Q2cum\cr
n: \tab the number of best compents\cr
pls1&pls2: \tab the PLS model of PLS regression\cr
Q2cum: \tab the Q2cum of PLS model\cr
vip: \tab R file contains vipmean and vipsd\cr
}
}


\author{
Jasper Shen

Maintainer: Jasper Shen <shenxt1990@163.com>
}
\references{
nothing
}

\keyword{ package }
\seealso{
\code{\link{plsr}}
\code{\link{plspedot}}
\code{\link{SXTvip}}
\code{\link{SXTscale}}
}
\examples{
nothing
}

