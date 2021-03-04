#' Check if a MAGIC design is balanced.
#'
#' This function takes a matrix of founder combinations and determines if the design
#' is balanced or not. The matrix rows are funnels and columns are founders.
#'
#' @param fmat a matrix of founder combinations.
#' @param verbose a logical indicator of whether verbose output is desired.
#' @return a logical indicator of whether the MAGIC design is balanced or not, and
#'         also a table of all founder pairings if `verbose=T`.
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=T)
#' magic.balanced(mpop$fcomb[[3]])
#' }
#'
#' @export

magic.balanced <- function(fmat, verbose=F){

  # get the number of founders and crossing generations.
  n0 <- ncol(fmat)
  n <- length(unique(fmat[1,]))
  nx <- log(n0, 2)
  
  # get the column indices to compare at each crossing generation.
  idx1 <- idx2 <- replicate(nx, vector())
  for(i in 1:nx){
    temp <- vector()
    for(j in seq(1, n0, 2^i)){
      temp <- c(temp, rep(j:(j+2^(i-1)-1), 2^(i-1)))
    }
    idx1[[i]] <- c(idx1[[i]], sort(temp))
    idx2[[i]] <- c(idx2[[i]], n0 - temp[length(temp):1] + 1)
  }

  # tabulate the number of founder pairs at each crossing generation.
  fpair <- t(combn(n, 2))
  fpair <- cbind(pair1=paste(fpair[,1], fpair[,2], sep="_"), pair2=paste(fpair[,2], fpair[,1], sep="_"))
  out <- vector()
  for(i in 1:nx){
    temp <- paste(c(fmat[, idx1[[i]]]), c(fmat[, idx2[[i]]]), sep="_")
    temp <- sapply(1:nrow(fpair), FUN=function(x) sum(temp%in%fpair[x,]))
    out <- cbind(out, temp)
  }
  rownames(out) <- fpair[,1]
  
  # if verbose=F, return TRUE/FALSE indicating whether the design is balanced or not.
  # if verbose=T, return also the counts of each founder pair.
  if(verbose){
    colnames(out) <- paste(2^c(1:nx), "way", sep="-")
    out <- list(nrow(unique(out))==1, out)
  } else {
    out <- nrow(unique(out))==1
  }
  return(out)
}