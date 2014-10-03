\name{crplot}
\alias{crplot}
\title{Component-plus-residual (CR) plot}
\description{Component-plus-residual (CR) plot for quantitative variables and group-specific effects.}
\usage{crplot(lm,quantv,g=NULL,newplot=TRUE,col=c(1,2,4),xlab=NULL,ylab='CR',...)}
\arguments{
  \item{lm}{object of class "lm". Fitted model.}
  \item{quantv}{character. Name of the quantitative independent variable ("component" to be plotted).}
  \item{g}{character vector. Interaction regressors coding for group-specific effects in the model.}
  \item{newplot}{logical. If TRUE (default) a new plot (graphics device) is opened.}
  \item{col}{integer vector. Colors of groups. Defaults to 1 (black), 2 (red) and 4 (blue) for group 1, 2 and 3, respectively.}
  \item{xlab}{character. x-axis label. Absent by default.}
  \item{ylab}{character. y-axis label. "CR" by default.}
  \item{...}{Further arguments passed to plot()}
}
\details{In the fitted model (lm), group-specific effects are specified by interaction regressors. In this case, an interaction regressor is a quantitative variable multiplied by a binary variable (i.e. where 0s represent samples of group 1 and 1s represent samples of group 2).

Predicted components are not centered and the intercept of the CR is 0 by definition. This function thus differs from cr.plot in the car package.
}
\value{
  \item{NULL}{This function is used for plotting.}
}
\references{Kuhn A, Thu D, Waldvogel HJ, Faull RL, Luthi-Carter R. Population-specific expression analysis (PSEA) reveals molecular changes in diseased brain. Nat Methods 2011, 8(11):945-7}
\author{Alexandre Kuhn \email{alexandre.m.kuhn@gmail.com}}
\examples{
## Load example expression data (variable "expression")
## for 23 transcripts and 41 samples, and associated
## phenotype (i.e. group) information (variable "groups")
data("example")

## The group data is encoded as a binary vector where
## 0s represent control samples (first 29 samples) and
## 1s represent disease samples (last 12 samples)
groups

## Neuronal reference signals (i.e. quantitative variable)
## and group-specific change in neuronal expression
## (i.e. interaction regressor)
neuron_probesets <- list(c("221805_at", "221801_x_at", "221916_at"),
                "201313_at", "210040_at", "205737_at", "210432_s_at")
neuron_reference <- marker(expression, neuron_probesets)
neuron_difference <- groups * neuron_reference

## Fit an expression model containing neuronal expression and
## neuron-specific change in expression between control and
## disease samples
model <- lm(expression["202429_s_at",] ~ neuron_reference +
		neuron_difference)

## Visualize the dependence on the neuronal reference signal and
## the group-specific effect (decreased neuronal expression in
## disease samples). Black and red dots represent control and
## disease samples, respectively.
crplot(model, "neuron_reference", g="neuron_difference")
}
\keyword{models}
