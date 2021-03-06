laser.distribution <-
function(x,id,percentiles=seq(0.1,0.9,0.1),gtv=gtv,prefix="H",suffix=""){
	skewness<-function(x){ 
		m3<-sum((x-mean(x))^3)/length(x) 
		s3<-sqrt(var(x))^3 
		m3/s3 
	} 
	kurtosis<-function(x) { 
		m4<-sum((x-mean(x))^4)/length(x) 
		s4<-var(x)^2 
		m4/s4 - 3 
	} 
	coefvar<-function(x){
		sd(x)/mean(x)
	}
	
	metrics <- function(x,gtv){
	  x <- x[x>=gtv]
	  
	  res <- c(
	  max=max(x),
	  mean=mean(x),
	  sd=sd (x),
	  cv=coefvar(x),
	  kurt=kurtosis (x),
	  skewness=skewness(x),
	  quantile(x,probs=percentiles, type=2)
	  )
	  return(res)}

	X <- aggregate(x,by=list(id=id),metrics,gtv)
	id <- X$id
	X <- as.data.frame(X$x)
	names(X) <- paste(prefix,gsub("[%]","",names(X)),suffix,sep="")
	X <- cbind(ID=id,X)
	return(X)
}

