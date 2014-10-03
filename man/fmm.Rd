\name{fmm}
\alias{fmm}
\title{Full model matrix}
\description{Generates a model matrix containing (quantitative) independent variables and interaction regressors coding for group-specific effects for each of tthe quantitative variable.}
\usage{fmm(v,d,int=TRUE)}
\arguments{
  \item{v}{n-by-m numeric matrix. Matrix containing m quantitative variables as columns (i.e. n is the number of samples).}
  \item{d}{n-by-o numeric matrix. Matrix containing o binary regressors (encoding o group-specific differences) as columns. n is the number of samples.}
  \item{int}{logical. If TRUE (default) an intercept is included as the first column of the full model matrix.}
}
\details{
Quantitative variables are specified via the argument "v". Sample groups are specified using binary (i.e. 0,1) variables (argument "d"). The returned model matrix represents a linear model with "m" quantitative variables and "o" group effects (corresponding to "o"+1 sample groups).

Specifically, the columns of the full model matrix contain (in this order): an intercept (if "int" is TRUE), the quantitative variables in the same order as they are provided in the input, group-specific effects for each quantitative variable (encoded as interaction regressors and in the same order as the quantitative variable are provided in the input).

An interaction regressor corresponds a quantitative variable multiplied by a (group-specific) binary variable. It encodes the difference (for the particular variable) between a specific group and the reference group.}
\value{
  \item{fmm}{numeric matrix. The full model matrix. Its dimension is is n-by-(m*(o+1)+1) if int is TRUE and n-by-(m*(o+1)) if int is FALSE.}
}
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

## Four cell population-specific reference signals
## (i.e. quantitative variable)
neuron_probesets <- list(c("221805_at", "221801_x_at", "221916_at"),
                "201313_at", "210040_at", "205737_at", "210432_s_at")
neuron_reference <- marker(expression, neuron_probesets)

astro_probesets <- list("203540_at",c("210068_s_at","210906_x_at"),
                "201667_at")
astro_reference <- marker(expression, astro_probesets)

oligo_probesets <- list(c("211836_s_at","214650_x_at"),"216617_s_at",
                "207659_s_at",c("207323_s_at","209072_at"))
oligo_reference <- marker(expression, oligo_probesets)

micro_probesets <- list("204192_at", "203416_at")
micro_reference <- marker(expression, micro_probesets)

## Full model matrix with an intercept, 4 quantitative variables and
## group-specific (disease vs control) differences for the
## 4 quantitative variables
fmm(cbind(neuron_reference, astro_reference, oligo_reference, 
	micro_reference), groups)
}
