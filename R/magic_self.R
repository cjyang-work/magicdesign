#' Add selfing after crossing.
#'
#' This function takes the "cross.info" object and add selfing after crosses. In the
#' usual design, the only selfing is the single-seed descent after the last crossing
#' generation.
#'
#' @param xinfo an object of "cross.info" class.
#' @param self a vector of number of selfing generations after each crossing generation.
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=T)
#' mpop <- magic.self(xinfo=mpop, self=c(0,0,3))
#' }
#'
#' @export

magic.self <- function(xinfo, self){

  # check if xinfo is an object of "cross.info" class.
  if(!is(xinfo, "cross.info")) stop("xinfo has to be an object of \"cross.info\" class.")

  # get the number of crossing generations.
  nx <- length(xinfo[[1]])

  # check if self is a vector of length nx.
  if(!is.vector(self)) stop("self has to be a vector.")
  if(!(length(self)==nx)) stop("self has to be a vector of length ", nx, ".")

  # add selfing to fcomb and xplan, if there is any.
  out <- list(list(), list())
  for(i in 1:nx){
    out[[1]] <- c(out[[1]], xinfo[[1]][i], replicate(self[i], xinfo[[1]][i]))
    out[[2]] <- c(out[[2]], xinfo[[2]][i], replicate(self[i], list(cbind(1:nrow(xinfo[[1]][[i]]), 1:nrow(xinfo[[1]][[i]])))))
  }

  names(out) <- c("fcomb", "xplan")
  class(out) <- "cross.info"
  
  return(out)
}