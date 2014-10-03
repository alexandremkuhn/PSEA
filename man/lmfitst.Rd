\name{lmfitst}
\alias{lmfitst}
\title{Fits a set of models and performs model selection.}
\description{For each response (dependent variable), the function fits a set of models and identify the best models based on AIC.}
\usage{lmfitst(y,fmdlm,st,subset=NULL,d=2,lm=TRUE)}
\arguments{
  \item{y}{numeric matrix or ExpressionSet. Matrix with response (dependent) variable as columns. If an ExpressionSet is provided the response matrix is extracted with the function exprs and transposed.}
  \item{fmdlm}{numeric matrix. Full model matrix with regressors as columns.}
  \item{st}{list of integer vectors. Each vector defines a model by specifying column indices in fmdlm.}
  \item{subset}{Integer vector. Subset of samples to be used for fitting. By default all samples are used.}
  \item{d}{numeric. Maximal distance (in AIC units) to the model with smallest AIC (defaults to 2). Models with an AIC difference less than or equal to d are returned.}
  \item{lm}{logical. If TRUE (default) the function also returns the fit (as returned by the function lm) obtained for the best model obtained for each response. If FALSE, the function only returns the indices (in st) corresponding to the selected models.}
}
\details{
This function is useful for performing model selection. It fits all possible models of a given set (instead of heuristically searching for the best one for instance) and compares them using Akaike Information Criterion (AIC).

For each response, all specified models are tested and all models obtaining an AIC within "d" AIC units of the model with smallest AIC are identified. These models are ordered by increasing number of regressors. Models with the same number of regressors are ordered by increasing AIC. The first model in this ordered list is considered the best model. Optionally the full fit (as returned by the function lm) of the best model is returned.

The formula for AIC follows the implementation of the function extractAIC and is: n * log(RSS / n) + 2 * edf, where n is the number of observations, RSS is the residual sum of squares and edf the number of free parameters.

Model fitting is implemented with lm.fit and NAs are not allowable in the response matrix y. 
}
\value{
  \item{wcrto}{list of integer vector (of length equal to the number of responses). Each integer vector represents the models (identified as indices into st) within d AIC units of the model with smallest AIC.}
  \item{ft}{list of lm objects. Each lm object contains the fit for the best model (defined as the model with the smallest number of regressors within d AIC units of the model with smallest AIC). Only returned if lm is TRUE (default).}
}
\seealso{
\code{\link{marker}},\code{\link{swlm}}.
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
model_matrix <- fmm(cbind(neuron_reference, astro_reference,
			oligo_reference, micro_reference), groups)

## Enumerate all possible models with any subset of the 4 reference signals
## (quantitiatve variables) and at most 1 group-specific effect
## (interaction regressor)
model_subset <- em_quantvg(c(2,3,4,5), tnv=4, ng=2)

## AIC-based model selection for 2 transcripts of interest
## (202429_s_at and 200850_s_at). For the first one, the selected
## model contains an intercept, the neuronal reference signal and
## a neuron-specific change (i.e. model 17 in model_subset
## corresponding to columns 1, 2 and 6 in model_matrix). For the
## second transcript, the selected model contains an intercept and
## the astrocytic reference signal (i.e. model 3 in model_subset
## corresponding to columns 1 and 3 in model_matrix)
lmfitst(t(expression[c("202429_s_at", "200850_s_at"),]),
	model_matrix, model_subset)
}
\keyword{models}
