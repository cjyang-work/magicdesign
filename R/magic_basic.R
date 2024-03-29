#' Create a basic MAGIC design.
#'
#' This function produces a single funnel of founder combinations and crosses for
#' 4 or more founders. Since there is only one funnel involved, the basic design
#' works the same regardless of whether the founders are inbred or not.
#'
#' @param n number of founders (4, 8, 16, 32, 64, 128).
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.basic(n=8)
#' }
#'
#' @export

magic.basic <- function(n){
  
  # check if n is within allowed values.
  if(!(n %in% c(4,8,16,32,64,128))) stop("n has to be a power of 2 and cannot be smaller than 4. e.g. 4, 8, 16, 32, 64, 128.")
  
  # get the number of crossing generations.
  nx <- log(n, 2)
  
  # create the founder combinations and crossing plans.
  fcomb <- lapply(0:nx, FUN=function(x) matrix(1:n, ncol=2^x, byrow=TRUE))
  xplan <- lapply(1:nx, FUN=function(x) matrix(1:nrow(fcomb[[x]]), ncol=2, byrow=TRUE))
  fcomb <- fcomb[-1]
  
  # create output and set class as "cross.info".
  out <- list(fcomb=fcomb,
              xplan=xplan)
  class(out) <- "cross.info"
  
  return(out)
  
}