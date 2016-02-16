#' Compute the number and proportion of returntypes
#' 
#' Read the header of a las-file into R  
#' 
#' @param dz surface height
#' @param RetNum return numb
#' @param NumRet number of returns 
#' @param id unique idenfitier for which variables will be aggregated
#' @return data.frame with the numbers and proportion of echoes in differnt categories.
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' data(las)
#' prop <- laser.returns(las$dz,las$r,las$n,id=las$ID)

laser.returns <- function(dz,RetNum,NumRet,id){
	echo <- laser.returntype (RetNum,NumRet)
	N <- data.frame(tapply(dz,list(id=id,echo=echo),length))
	N[is.na(N)]<-0
	N$FS <- N$FIRST + N$SINGLE
	N.prop <- N[,1:4]/N$FS
	names(N)<- paste(names(N),".n",sep="")
	names(N.prop)<- paste(names(N.prop),".p",sep="")
	N <- cbind(ID=row.names(N),N,N.prop)
	#N$ID <- row.names(N)
	return(N)
}

# echo.cat <- function(RetNum,NumRet){
#   k <- c()
#   k[RetNum == 1 & RetNum == NumRet]<- "SINGLE"
#   k[RetNum == 1 & RetNum != NumRet]<- "FIRST"
#   k[RetNum != 1 & RetNum == NumRet]<- "LAST"
#   k[RetNum != 1 & RetNum != NumRet]<- "INTER"
#   return(k)
# }
