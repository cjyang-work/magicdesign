#' Add replicates to crosses.
#'
#' This function takes the "cross.info" object and add replicates to the crosses.
#'
#' @param xinfo an object of "cross.info" class.
#' @param reps a vector of replicates in each crossing generation.
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=TRUE)
#' mpop <- magic.reps(xinfo=mpop, reps=c(1,1,2))
#' }
#'
#' @export

magic.reps <- function(xinfo, reps){

  # check if xinfo is an object of "cross.info" class.
  if(!methods::is(xinfo, "cross.info")) stop("xinfo has to be an object of \"cross.info\" class.")

  # get the number of crossing generations.
  nx <- length(xinfo[[1]])

  # check if reps is a vector of length nx.
  if(!is.vector(reps)) stop("reps has to be a vector.")
  if(!(length(reps)==nx)) stop("reps has to be a vector of length(xinfo[[1]]).")
  
  # calculate the cumulative replicates.
  repc <- cumprod(reps)
  
  # add the replicates to fcomb.
  fcomb <- xinfo[[1]]

  for(i in 1:nx){
    rownames(fcomb[[i]]) <- 1:nrow(fcomb[[i]])
    fcomb[[i]] <- do.call(rbind, replicate(repc[i], list(fcomb[[i]])))
    fcomb[[i]] <- fcomb[[i]][order(as.numeric(rownames(fcomb[[i]]))), , drop=FALSE]
    attr(fcomb[[i]], "dimnames") <- NULL
  }
  
  # calculate the cumulative replicates, but separate by each reps.
  repc2 <- list()
  for(i in 1:nx){
      temp <- rep(1,nx)
      temp[i] <- reps[i]
      repc2 <- c(repc2, list(cumprod(temp)))
  }
  
  # add replicates to xplan.
  xplan <- xinfo[[2]]
  for(i in 1:nx){
    for(j in i:nx){
      if(j == i){
        rownames(xplan[[j]]) <- 1:nrow(xplan[[j]])
        xplan[[j]] <- do.call(rbind, replicate(reps[i], list(xplan[[j]])))
        xplan[[j]] <- xplan[[j]][order(as.numeric(rownames(xplan[[j]]))), , drop=FALSE]
        attr(xplan[[i]], "dimnames") <- NULL
      } else {
        rownames(xplan[[j]]) <- 1:nrow(xplan[[j]])
        temp <- lapply(1:reps[i], FUN=function(x) (xplan[[j]] - 1)*reps[i] + x )
        xplan[[j]] <- do.call(rbind, temp)
        xplan[[j]] <- xplan[[j]][order(as.numeric(rownames(xplan[[j]]))), , drop=FALSE]
        attr(xplan[[i]], "dimnames") <- NULL
      }
    }
  }
  
  out <- list(fcomb=fcomb,
              xplan=xplan)
  class(out) <- "cross.info"
  
  return(out)
  
}