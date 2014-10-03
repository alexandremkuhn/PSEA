fmm<-function(v,d,int=TRUE) {
        if (is.null(dim(v))) v<-matrix(v,length(v))
        if (is.null(dim(d))) d<-matrix(d,length(d))
        if (nrow(v)!=nrow(d)) stop("v and d have different numbers of observations.")
        fmm<-matrix(NA,nrow(v),ncol(v)*(1+ncol(d)))
        fmm[,1:ncol(v)]<-v
        for (i in 1:ncol(d)) {
                fmm[,(1+(i*ncol(v))):((i+1)*ncol(v))]<-v*as.vector(d[,i])
        }
        if (int) fmm<-cbind(1,fmm)
        return(fmm)
}
