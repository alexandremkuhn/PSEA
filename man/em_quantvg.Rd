\name{em_quantvg}
\alias{em_quantvg}
\title{Specifies particular models useful for gene expression modeling}
\description{Enumerates sets of integer that specify variables to include in each model of the family.}
\usage{em_quantvg(vi,tnv=NULL,ng=1,sk=NULL,mnr=NULL,int=TRUE)}
\arguments{
  \item{vi}{integer vector. Indices of the quantitative variables in the full model matrix.}
  \item{tnv}{integer. Total number of quantitative variables (excluding the intercept) in the full model matrix (if (int=TRUE) tnv>=(max(vi)-1). If (int=FALSE) tnv>=max(vi)). By default: tnv=(max(vi)-1) if (int=TRUE) and tnv=max(vi) if (int=FALSE).}
  \item{ng}{integer. Number of samples groups: 1 (default), 2 or 3.}
  \item{sk}{character. Indicates if the models with group-specific regressors defining group 2 ("skip2") should be eliminated or if models containing group-specific regressors for groups 2 and 3 (simultaneously) should be prohibited ("skip23"). These apply only if ng=3.}
  \item{mnr}{integer. Maximal total number of regressors (including the intercept if "int" is TRUE) in the enumerated models.}
  \item{int}{logical. If TRUE (default) the first column of the full model matrix is assumed to be an intercept.}
}
\details{
We specify a family of linear models corresponding to a (multiple) regression on selected quantitative variables and up to 3 different treatment groups. The function enumerates all subset models with up to 1 (i.e. 0 or 1) group-specific change in any of selected the quantitative variables. Regressors coding for a group-specific change in a given quantitative variable are only allowed if the correponding quantitative variable is present in the model.

The function returns sets of integers that define models by specifying indices to the columns of a full model matrix. The full model matrix can contain more quantitative variables than those selected for building the enumeration.

The full model matrix (with columns as regressors) is assumed to contain all quantitative variables first, followed by the interaction regressors coding for differences between sample group 1 and 2, possibly followed by the interaction regressors coding for differences between group 1 and 3. The maximal number of sample groups is 3. The order of interaction regressors for a given group is assumed to be the same as for the quantitative variables.

The first column of the full model matrix is assumed to be an intercept. If the full model matrix does not contain an intercept, the argument "int" should be set to FALSE (and the returned models will not include an intercept).
}
\value{
  \item{mi}{list of integer vectors. Each vector defines a model by specifying column indices in the full model matrix.}
}
\seealso{
  \code{\link{fmm}}.
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
model_matrix <- fmm(cbind(neuron_reference,astro_reference,
			oligo_reference, micro_reference), groups)

## Enumerate all possible models with any subset of the 4 reference signals
## (quantitiatve variables) and at most 1 group-specific effect
## (interaction regressor)
model_subset <- em_quantvg(c(2,3,4,5), tnv=4, ng=2)

## There are 48 models
length(model_subset)

## For instance the 17th model in the list contains an intercept (column 1 in model_matrix), the neuronal reference signal (column 2) and the neuron-specific change across groups (column6)
model_subset[[17]]
}
