slt<-function(ls,slot,index=NULL) {
        ind<-which(names(ls[[1]])==slot);
        if (is.null(index)) sc<-sapply(ls,function(x){x[[ind]]})
        else sc<-sapply(ls,function(x){x[[ind]][index] })
	return(sc)
}
