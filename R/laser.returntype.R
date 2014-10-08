#' Classify returntypes, Echo categories
#' 
#' Read the header of a las-file into R  
#' 
#' @param z surface height
#' @param RetNum return numb
#' @param NumRet number of returns 
#' @param id 
#' @return data.frame with the numbers and proportion of echoes in differnt categories.
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' data(las)
#' prop <- laser.returns(las$dz,las$r,las$n,ID)
#' Echo.cat <- laser.returntype(las$r,las$n)


laser.returntype <- function(RetNum,NumRet){
  k <- c()
  k[RetNum == 1 & RetNum == NumRet]<- "SINGLE"
  k[RetNum == 1 & RetNum != NumRet]<- "FIRST"
  k[RetNum != 1 & RetNum == NumRet]<- "LAST"
  k[RetNum != 1 & RetNum != NumRet]<- "INTER"
  k[RetNum == 0 | NumRet == 0]<- "ZERO"
  k[NumRet < RetNum]<- "ORDER"
  return(k)
}