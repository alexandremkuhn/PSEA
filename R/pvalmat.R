pvalmat<-function(lst,regressors) {
        pvalm<-matrix(NA,length(lst),length(regressors))
        colnames(pvalm)<-regressors
        for (i in 1:length(lst)) {
                coef<-summary(lst[[i]])$coef
		logind<-rownames(coef) %in% regressors
                pvalm[i,rownames(coef)[logind]]<-coef[logind,4]
        }
        colnames(pvalm)<-paste('pvalue',regressors,sep='.')
        return(pvalm)
}
