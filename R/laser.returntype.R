#' Classify returntypes, Echo categories
#' 
#' Classify echoes into echo categories based on return number and number of returns
#' 
#' @param RetNum return numb
#' @param NumRet number of returns 
#' @return vector with echo categories "SINGLE" (first and only), "FIRST" (first of many), "LAST"(last of many), "INTER" (intemediate), "ZERO" (labeling errors). 
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' data(las)
#' Echo.cat <- laser.returntype(las$r,las$n)
#' table(Echo.cat)
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

