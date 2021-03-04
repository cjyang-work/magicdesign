#' Extract the founder genotypes.
#'
#' This function takes haplotypes from simulation and converts them into
#' founder genotypes.
#'
#' @param hap a matrix of haplotypes in the RILs.
#' @param n number of founders.
#' @return a matrix of founder genotypes in the RILs.
#'
#' @noRd

magic.hap <- function(hap, n){

  # get the marker indices that correspond to each founder allele.
  idx <- lapply(1:(n-1), FUN=function(x) seq(x, ncol(hap), n-1))

  # create a new haplotype matrix.
  out <- matrix(1, nrow=nrow(hap), ncol=ncol(hap)/(n-1))

  # convert the haplotypes into parental genotypes.
  for(i in 1:ncol(out)){
    for(j in 1:length(idx)){
      out[hap[,idx[[j]][i]]==1,i] <- j + 1
    }
  }
  
  return(out)
  
}