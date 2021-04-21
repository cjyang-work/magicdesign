#' Minimize the crosses.
#'
#' This function takes the "cross.info" object and removes any redundant crosses
#' from a full/partial/deficient design to minimize crossing, i.e. consolidates 
#' multiple same crosses to one.
#'
#' @param xinfo an object of "cross.info" class.
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @noRd

magic.minimize <- function(xinfo){

  # check if xinfo is an object of "cross.info" class.
  if(!methods::is(xinfo, "cross.info")) stop("xinfo has to be an object of \"cross.info\" class.")

  # get the founder combination and number of crossing generations.
  fcomb <- xinfo[[1]]
  nx <- length(fcomb)

  # remove redundant crosses.
  for(i in 1:nx){
    fcomb[[i]] <- unique(fcomb[[i]])
  }

  # adjust the crossing plan after removing redundant crosses.
  xplan <- replicate(nx, vector())
  xplan[[1]] <- fcomb[[1]]

  for(i in 2:nx){
    for(j in 1:nrow(fcomb[[i]])){
      xplan[[i]] <- rbind(xplan[[i]], c(which(colSums(t(fcomb[[i-1]])==fcomb[[i]][j,1:ncol(fcomb[[i-1]])])==ncol(fcomb[[i-1]])),
                                        which(colSums(t(fcomb[[i-1]])==fcomb[[i]][j,(ncol(fcomb[[i-1]])+1):ncol(fcomb[[i]])])==ncol(fcomb[[i-1]]))))
    }
  }
  
  out <- list(fcomb=fcomb,
              xplan=xplan)
  class(out) <- "cross.info"
  
  return(out)
  
}