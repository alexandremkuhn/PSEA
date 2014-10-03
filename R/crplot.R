crplot<-function(lm,quantv,g=NULL,newplot=TRUE,col=c(1,2,4),xlab=NULL,ylab='CR',...){

        if (is.na(match(quantv,colnames(lm$model)))) stop(quantv,' not found in model.')
        if (!is.null(g)) {
                if (any(is.na(match(g,colnames(lm$model))))) stop('(One or more of) ',paste(g,collapse=", "),' not found in model.')
                indg<-list()
                for (i in 1:length(g)) indg[[i]]<-which(lm$model[,g[i]]!=0)
        }
        col<-rep(col,length=1+length(g))

        rpc<-residuals(lm) + as.matrix(lm$model[,c(quantv,g)]) %*% lm$coef[c(quantv,g)]

        if (newplot) dev.new()
        if (is.null(g)) {
                plot(lm$model[,quantv],rpc,col=col[1],xlab=if (is.null(xlab)) quantv else xlab,ylab=ylab, ...)
                abline(0,lm$coef[quantv],col=col[1])
        }
        else {
                plot(lm$model[,quantv],rpc,xlab=if (is.null(xlab)) quantv else xlab,ylab=ylab,type='n',...)
                points(lm$model[-unlist(indg),quantv],rpc[-unlist(indg)],col=col[1])
                abline(0,lm$coef[quantv],col=col[1])
                for (i in 1:length(g)) {
                        points(lm$model[indg[[i]],g[i]],rpc[indg[[i]]],col=col[i+1])
                        abline(0,sum(lm$coef[c(quantv,g[i])]),col=col[i+1])
                }
        }
}

