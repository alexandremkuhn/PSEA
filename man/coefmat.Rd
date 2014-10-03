\name{coefmat}
\alias{coefmat}
\title{Extracts fitted coefficients.}
\description{Takes a list of fitted models (lm objects) and returns coefficients in matrix format.}
\usage{coefmat(lst,regressors)}
\arguments{
  \item{lst}{list of lm objects.}
  \item{regressors}{character vector. Names of the coefficients to extract.}
}
\details{
Simple wrapper function that returns fitted coefficients.

The names of the coefficient to extract are matched in names(lst[[i]]$coef).

The column in the matrix of extracted coefficients are named by prepending "coef." to the regressor names.
}
\value{
  \item{coefm}{numeric matrix. Matrix of extracted coefficients with one row for each model in the list and one column for each targeted coefficient.}
}
\seealso{
  \code{\link{pvalmat}}.
}
\author{Alexandre Kuhn \email{alexandre.m.kuhn@gmail.com}}
\examples{
## Load example expression data (variable "expression")
## and phenotype data (variable "groups")
data("example")

## Four cell population-specific reference signals
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

## Stepwise model selection for 2 transcripts (202429_s_at and
## 200850_s_at) and focusing on control samples (i.e. groups == 0)
lmlist <- swlm(expression[c("202429_s_at", "200850_s_at"),],
		subset = which(groups == 0),
		upper = formula(~neuron_reference + astro_reference +
			oligo_reference + micro_reference))

coefmat(lmlist, c("(Intercept)", "neuron_reference", "astro_reference",
		"oligo_reference", "micro_reference"))
}
\keyword{models}
