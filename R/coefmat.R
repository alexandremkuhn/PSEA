coefmat<-function(lst,regressors) {
        coefm<-matrix(NA,length(lst),length(regressors))
        colnames(coefm)<-regressors
        for (i in 1:length(lst)) {
		logind<-names(lst[[i]]$coef) %in% regressors
                coefm[i,names(lst[[i]]$coef)[logind]]<-lst[[i]]$coef[logind]
        }
        colnames(coefm)<-paste('coef',regressors,sep='.')
        return(coefm)
}
