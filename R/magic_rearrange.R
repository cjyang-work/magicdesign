#' Rearrange the founder combinations.
#'
#' This function rearranges a matrix of founder combinations such that smaller
#' founder appears before larger founder in any cross (e.g. 3124 becomes 1324).
#'
#' @param fmat matrix of founder combinations.
#' @return an object of rearranged matrix of founder combinations.
#'
#' @examples
#' \donttest{
#' mpop <- t(sapply(1:3, FUN=function(x) sample(1:8, 8)))
#' mpop <- magic.rearrange(fmat=mpop)
#' }
#'
#' @export

magic.rearrange <- function(fmat){

  # get the number of founders and crossing generation.
  n <- ncol(fmat)
  nx <- log(n, 2)
  
  # check if n is a power of 2.
  if(!(nx%%1==0)) stop("n has to be a power of 2. e.g. 2, 4, 8.")

  # rearrange fmat.
  fmat <- c(t(fmat))
  fmat <- sapply(1:length(fmat), FUN=function(x) paste(rep(0, nchar(max(fmat))-nchar(fmat[x])), fmat[x], sep=""))
  fmat <- formatC(fmat, width=nchar(max(fmat)), flag="0")
  fmat <- matrix(fmat, nrow=n)
  
  for(i in 1:nx){
    fmat <- matrix(fmat, nrow=2^i)
    temp <- fmat
    z1 <- as.numeric(do.call(paste, c(data.frame(t(fmat[1:2^(i-1), , drop=F])), sep="")))
    z2 <- as.numeric(do.call(paste, c(data.frame(t(fmat[(1+2^(i-1)):nrow(fmat), , drop=F])), sep="")))
    
    if(any(z2 < z1)){
      fmat[1:2^(i-1), z2 < z1] <- temp[(1+2^(i-1)):nrow(temp), z2 < z1]
      fmat[(1+2^(i-1)):nrow(fmat), z2 < z1] <- temp[1:2^(i-1), z2 < z1]
    }
    
  }
  
  fmat <- t(matrix(as.numeric(c(fmat)), nrow=n))
  
  
  return(fmat)
}