
rbeta.mom.int <- function(mean.est,sd.est){
	P=mean.est*(mean.est*(1-mean.est)/sd.est^2-1)
	Q=(1-mean.est)*(mean.est*(1-mean.est)/sd.est^2-1)
	retval <- rbeta(1,P,Q)
	if(is.nan(retval)){
		retval <- mean.est
	}
	return(retval)
}

rbeta.mom <- Vectorize(rbeta.mom.int)














