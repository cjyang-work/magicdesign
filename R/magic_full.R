#' Create a full MAGIC design.
#'
#' This function produces all possible founder combinations and crosses for
#' only either 4 or 8 founders. In a full design, the number of unique
#' founder combinations (funnels) is given by \eqn{n!/2^{n-1}}. Full design for
#' 16 or more founders are impossible in practice, for example, a full design
#' for 16 founders would have 638,512,875 funnels.
#'
#' @param n number of founders (4, 8).
#' @param inbred logical indicator of whether the founders are inbred.
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.full(n=4)
#' }
#'
#' @export

magic.full <- function(n, inbred=TRUE){

  # check if n is within allowed values.
  if(!(n %in% c(4,8))) stop("n has to be either 4 or 8.")
  
  # argument check: inbred.
  if(!is.logical(inbred)) stop("argument \"inbred\" has to be either TRUE (T) or FALSE (F).")

  # get the number of crossing generations.
  nx <- log(n, 2)
  
  # identify all n-way combinations.
  temp <- replicate(nx+1, vector())
  temp[[1]] <- matrix(1:n, ncol=1)

  for(k in 2:(nx+1)){
    for(i in 1:(nrow(temp[[k-1]])-1)){
      for(j in (i+1):nrow(temp[[k-1]])){
        if(!any(temp[[k-1]][i,]%in%temp[[k-1]][j,])) {
          temp[[k]] <- rbind(temp[[k]], c(temp[[k-1]][i,], temp[[k-1]][j,]))
        }
      }
    }
  }
  
  # now we have the final founder combinations, we back-calculate the crosses for earlier generations.
  fcomb <- replicate(nx, list())
  xplan <- replicate(nx, list())
  
  fcomb[[nx]] <- temp[[nx+1]]
  
  for(i in (nx-1):1){
    fcomb[[i]] <- rbind(fcomb[[i+1]][,1:(ncol(fcomb[[i+1]])/2)],
                        fcomb[[i+1]][,(ncol(fcomb[[i+1]])/2 + 1):ncol(fcomb[[i+1]])])
  }
  
  # remove unnecessary 2-ways if the founders are inbred.
  if(inbred){
    fcomb[[1]] <- unique(fcomb[[1]])
  }
  
  xplan[[1]] <- fcomb[[1]]
  
  for(i in 2:nx){
    xplan[[i]] <- matrix(1:nrow(fcomb[[i-1]]), ncol=2)
  }
  
  # correct the xplan[[2]] if the founders are inbred.
  if(inbred){
    temp1 <- temp2 <- vector()
    for(i in 1:nrow(fcomb[[2]])){
      temp1 <- c(temp1, which(colSums(t(fcomb[[1]])==fcomb[[2]][i,1:2])==2))
      temp2 <- c(temp2, which(colSums(t(fcomb[[1]])==fcomb[[2]][i,3:4])==2))
    }
    xplan[[2]] <- cbind(temp1,temp2)
    attr(xplan[[2]], "dimnames") <- NULL
  }

  out <- list(fcomb=fcomb,
              xplan=xplan)
  class(out) <- "cross.info"

  return(out)

}