\name{slt}
\alias{slt}
\title{Extracts from a list of objects}
\description{Simple wrapper to extract a particular slot for each object in a list.}
\usage{slt(ls,slot,index=NULL)}
\arguments{
  \item{ls}{list of objects.}
  \item{slot}{character. Name of the slot to extract.}
  \item{index}{integer vector. Optional index vector to extract only a subset of the elements contained in the slot.}
}
\details{
The slot to be extracted is defined by matching "slot" against slot names of the first object in the list.

Slot extraction is done with sapply and the format of the output will depend on the content of the slots (see the value returned by sapply for details).
}
\value{
  \item{sc}{vector or matrix.}
}
\author{Alexandre Kuhn \email{alexandre.m.kuhn@gmail.com}}
\examples{
## Two arbitrary regression summaries in a list
x<-1:10
y1 <- x + rnorm(10,sd=1)
y2 <- x + rnorm(10,sd=5)

slm<-list()
slm[[1]] <- summary(lm(y1 ~ x))
slm[[2]] <- summary(lm(y2 ~ x))

## Extract R-squared from the list of summary.lm objects
slt(slm, "adj.r.squared")
}
