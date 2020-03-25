swlm<-function(y,subset=NULL,upper,lower=formula(~1),direction="both",trace=FALSE,keep=NULL,verbose=FALSE) {
	if (is(y,"ExpressionSet")) y<-exprs(y)
        if (is.null(dim(y))) y<-matrix(y,nrow=1)
        if (is.null(subset)) subset<-1:ncol(y)
        swft<-list()
        for (i in 1:nrow(y)) {
                if (verbose) message("Fitting response ",i)
                ft<-lm(y[i,]~1,subset=subset)
                swft[[i]]<-stepAIC(ft,scope=list(upper=upper,lower=lower),
				direction='both',trace=trace,keep=keep)
        }
        return(swft)
}
