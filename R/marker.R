marker<-function(expr,id,sampleSubset=NULL,targetMean=1){
	if (class(expr)=="ExpressionSet") expr<-exprs(expr)
	if (!is.matrix(expr)) {
		warning("expr is not a matrix. Coercing to matrix.")
		expr<-as.matrix(expr)
	}
	if (any(sapply(id,is.null))) stop("An element of id is null.")
        if (is.null(sampleSubset)) sampleSubset<-1:ncol(expr)
	if (any(is.na(sampleSubset))) stop("NA in sampleSubset.")	

	mrkrs<-sapply(id,function(x) {
		xind<-match(x,rownames(expr))
		if (any(is.na(xind))) stop("Unmatched marker.") else
		apply(targetMean*expr[xind,,drop=FALSE]/
		apply(expr[xind,sampleSubset,drop=FALSE],1,mean,na.rm=TRUE),
		2,mean,na.rm=TRUE)
		})

	refSignal<-apply(t(mrkrs),2,mean)

	return(refSignal)
}
