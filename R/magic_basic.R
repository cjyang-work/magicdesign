#' Create a basic MAGIC design.
#'
#' This function produces a single funnel of founder combinations and crosses for
#' 4 or more founders. Since there only one funnel, there is no need for
#' specifying if the founders are inbred or not.
#'
#' @param n number of founders.
#' @return an object of "cross.info" class, which is a list of
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
  fcomb <- lapply(0:nx, FUN=function(x) matrix(1:n, ncol=2^x, byrow=T))
  xplan <- lapply(1:nx, FUN=function(x) matrix(1:nrow(fcomb[[x]]), ncol=2, byrow=T))
  fcomb <- fcomb[-1]
  
  # create output and set class as "cross.info".
  out <- list(fcomb=fcomb,
              xplan=xplan)
  class(out) <- "cross.info"
  
  return(out)
  
}