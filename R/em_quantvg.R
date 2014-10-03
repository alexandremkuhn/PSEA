em_quantvg<-function(vi,tnv=NULL,ng=1,sk=NULL,mnr=NULL,int=TRUE){
        if (int & any(vi<2)) stop("Intercept (encoded as 1) included in the quantitative variables")
        # principle: specify regressors with intercept, enumerate with intercept
        # and add intercept after enumeration
        if (int) vi<-vi-1

        if (is.null(tnv)) tnv<-max(vi) else if (tnv<max(vi)) stop("The total number of quantitative variables (tnv) cannot be less than the largest index of the selected quantitative variables (vi).")
        if (!is.element(ng,1:3)) stop("Number of sample groups can only be 1, 2, or 3.")
        if (!is.null(mnr)) {if (mnr<1) stop("Maximal number of regressors is not striclty positive.")}
        if (!is.null(sk)) {if (!is.element(sk,c("skip2","skip23"))) stop("sk must be 'skip2' or 'skip23'.") else if (ng!=3) stop("sk can be specified only with 3 sample groups (ng=3).")}

        emg1<-unlist(lapply(1:length(vi),function(x){
		combn2(vi,x,simplify=FALSE)}),recursive=FALSE)
        if (ng>1) {
		emg2<-unlist(lapply(emg1,function(x){
			g2<-combn2(x,1,simplify=FALSE);
			lapply(g2,function(y){c(x,tnv+y)})}),
			recursive=FALSE)}
        if (ng>2) {
                emg3<-unlist(lapply(emg1,function(x){
			g3<-combn2(x,1,simplify=FALSE);
			lapply(g3,function(y){c(x,2*tnv+y)})}),
			recursive=FALSE)
                emg23<-unlist(lapply(emg1,function(x){
			g3<-combn2(x,1,simplify=FALSE);
			lapply(g3,function(y){c(x,tnv+y,2*tnv+y)})}),
			recursive=FALSE)}
        if (!is.null(sk) && ng==3) emg<-switch(sk,skip2=c(emg1,emg3),
					skip23=c(emg1,emg2,emg3))
	else emg<-switch(ng,emg1,c(emg1,emg2),c(emg1,emg2,emg3,emg23))

        if (int) emg<-c(list(1),lapply(emg,function(x){c(1,x+1)}))

        if (is.null(mnr)) return(emg) else
		return(emg[!(sapply(emg,length)>mnr)])
}
