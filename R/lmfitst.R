lmfitst<-function(y,fmdlm,st,subset=NULL,d=2,lm=TRUE) {
	if (class(y)=="ExpressionSet") y<-t(exprs(y))
        if (is.null(dim(y))) stop("y (or exprs(y) if y is an ExpressionSet) is not a matrix")
        if (!is.null(subset)) {y<-y[subset,,drop=FALSE]
				fmdlm<-fmdlm[subset,,drop=FALSE]}
        n<-nrow(y)
        ivn<-ncol(y)
        rgrsrnb<-sapply(st,length)
        stft<-lapply(st,function(x){lm.fit(fmdlm[,x,drop=FALSE],y)})
	##AIC as in extractAIC() (not as in AIC())
	crtst<-sapply(stft,function(x){
		edf<-n-x$df.residual
		n*log(apply(matrix(x$residuals,nrow=n)^2,2,sum)/n)+2*edf})
        if (is.null(dim(crtst))) crtst<-matrix(crtst,nrow=1) #if only 1 response
        wcrt<-apply(crtst,1,function(x){which(x-min(x)<=d)})
        if (!is.list(wcrt)) wcrt<-as.list(unname(data.frame(wcrt))) #if same number
        wcrto<-lapply(1:ivn,function(x){wcrt[[x]][
                        order(rgrsrnb[wcrt[[x]]],crtst[x,wcrt[[x]]])]})
        wcrto1<-sapply(wcrto,function(x){x[1]})
        if (lm) { #use lm with intercept or R2 is wrong
                ft<-lapply(1:ivn,function(x){ #assumes interc is the only set of length 1
			if (length(st[[wcrto1[x]]])==1) lm(y[,x]~1)
			else lm(y[,x]~.,
				data=data.frame(fmdlm[,st[[wcrto1[x]]][-1]]))})
                if (!is.null(colnames(y))) names(ft)<-colnames(y)
                for (i in 1:ivn) { #change regressor names too
                        names(ft[[i]]$coefficients)<-as.character(
							st[[wcrto1[i]]])
                        colnames(ft[[i]]$model)<-c(paste("y[,",i," ]",sep=""),
					as.character(st[[wcrto1[i]]][-1]))
                }
                return(list(wcrto,ft))
                }
        else return(list(wcrto))
}
