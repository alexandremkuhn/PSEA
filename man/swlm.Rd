\name{swlm}
\alias{swlm}
\title{Stepwise model selection}
\description{Simple wrapper around stepAIC() (package MASS) to repeatedly perform stepwise model selection by AIC on several dependent variables (or responses, taken as rows of a matrix).}
\usage{swlm(y,subset=NULL,upper,lower=formula(~1),direction='both',trace=FALSE,keep=NULL,verbose=FALSE)}
\arguments{
  \item{y}{Numeric matrix (with responses as rows and samples as columns) or ExpressionSet. Typically the expression data with transcripts (i.e. for a microarray, probes) as rows and samples as columns. If an ExpressionSet is provided the expression data is extracted with the function exprs.}
  \item{subset}{Integer vector. Represents a subset of samples (specified
as column indices in y) to use for model fitting. By default all samples are used.}
  \item{verbose}{logical. If TRUE (default) the response number being fitted is printed.}
  \item{upper}{see ?stepAIC}
  \item{lower}{see ?stepAIC}
  \item{direction}{see ?stepAIC}
  \item{trace}{see ?stepAIC}
  \item{keep}{see ?stepAIC}
}
\details{
The initial model for the stepwise approach only contains an intercept term.
}
\value{
  \item{swft}{List of stepwise-selected models (see ?stepAIC)}
}
\seealso{
\code{\link{marker}},\code{\link{lmfitst}}.
}
\references{Kuhn A, Kumar A, Beilina A, Dillman A, Cookson MR, Singleton AB. Cell population-specific expression analysis of human cerebellum. BMC Genomics 2012, 13:610.}
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

## Stepwise model selection for 2 transcripts (202429_s_at and 200850_s_at)
## and focusing on control samples (i.e. groups == 0)
swlm(expression[c("202429_s_at", "200850_s_at"),],
	subset = which(groups == 0), 
	upper = formula(~neuron_reference + astro_reference +
			oligo_reference + micro_reference))
}
\keyword{models}
