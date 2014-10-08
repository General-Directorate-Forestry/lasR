# TODO: Add comment
# 
# Author: hanso
###############################################################################


laser.density <-
  function(x,id,gtv=gtv,ctv = function(x){quantile(x[x>=gtv],probs=0.95,type=2)},prefix="D",suffix=""){#
    DensityMetrics <- function(x){  
      if(is.na(ctv(x))==FALSE){
        k<-vector()
        for (i in 0:9){
          k<-cbind(k,(length(x[x >= gtv+((ctv(x)-gtv)/10)*i])/length(x))) 
        }
        k<-as.numeric(k)
    }
      else
       {
          k <- rep(0,10)
      }
    }
      X <- aggregate(x,by=list(id=id),DensityMetrics)
      id <- X$id
      X <- as.data.frame(X$x)
      names(X) <- paste(prefix,c('0','1','2','3','4','5','6','7','8','9'),suffix,sep="")
      X <- cbind(ID=id,X)
      return(X)
    }


