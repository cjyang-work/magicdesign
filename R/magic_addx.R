#' Include additional crossing generations.
#'
#' This function takes the "cross.info" object and include an additional crossing
#' generation after the final n-way crosses have been made.
#'
#' @param xinfo an object of "cross.info" class.
#' @param addx an integer of either 1 or 2 indicating the type of additional crosses.
#' @param repx an integer of number of replicates from the additional crosses.
#' @param selfx an integer of number of selfing generations after the additional crosses.
#' @return an object of "cross.info" class, which is a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @details 
#' There are two options available: addx=1 splits the n-way individuals into two equal pools
#' and make all possible crosses between these two pools; addx=2 keeps the n-way individuals
#' in one pool and make all possible crosses within this one pool (random mating). Note
#' that addx=1 is only available for basic design, and while addx=2 is not restricted
#' to just basic design, it is still not recommended for other designs.
#'
#' @examples
#' \donttest{
#' mpop <- magic.basic(n=8)
#' mpop <- magic.reps(xinfo=mpop, reps=c(1,4,1))
#' mpop <- magic.addx(xinfo=mpop, addx=1, repx=3, selfx=3)
#' }
#'
#' @export

magic.addx <- function(xinfo, addx, repx, selfx){

  # check if xinfo is an object of "cross.info" class.
  if(!is(xinfo, "cross.info")) stop("xinfo has to be an object of \"cross.info\" class.")
  
  # check if addx, repx and selfx are within allowed values.
  if(!(addx %in% c(1,2))) stop("addx has to be either 1 or 2.")
  if(!(repx %% 1 == 0) | repx < 1) stop("repx has to be a positive integer.")
  if(!(selfx %% 1 == 0) | selfx < 0) stop("selfx has to be a non-negative integer.")
  
  # get the total number of generations so far and last founder combination.
  n.gen <- length(xinfo[[1]])
  fmat <- xinfo[[1]][[n.gen]]
  
  # add an additional crossing generation among the n-way lines if addx is either 1 or 2.
  if(addx == 1 & nrow(unique(fmat)) == 1 & nrow(fmat)%%2 == 0){
    
    temp <- t(sapply(1:(nrow(fmat)/2)^2, FUN=function(x) fmat[1,,drop=F]))
    xinfo[[1]] <- c(xinfo[[1]], list(cbind(temp, temp)))
    xinfo[[2]] <- c(xinfo[[2]], list(cbind(sort(rep(1:(nrow(fmat)/2), nrow(fmat)/2)),
                                       rep((nrow(fmat)/2 + 1):nrow(fmat), nrow(fmat)/2))))
    
  } else if(addx == 2 & nrow(fmat) > 1){
    
    temp <- t(combn(nrow(fmat), 2))
    xinfo[[1]] <- c(xinfo[[1]], list(cbind(fmat[temp[,1], , drop=F], fmat[temp[,2], , drop=F])))
    xinfo[[2]] <- c(xinfo[[2]], list(temp))
    
  } else {
    
    stop("\"addx\" is not available for this design, please revise.")
    
  }

  # update to the latest total number of generations and last founder combination.
  n.gen <- length(xinfo[[1]])
  fmat <- xinfo[[1]][[n.gen]]
  
  # add repx to the additional generation.
  if(repx > 1){
    rownames(fmat) <- 1:nrow(fmat)
    fmat <- do.call(rbind, replicate(repx, list(fmat)))
    fmat <- fmat[order(as.numeric(rownames(fmat))), , drop=F]
    attr(fmat, "dimnames") <- NULL
    xinfo[[1]][[n.gen]] <- fmat
    
    rownames(xinfo[[2]][[n.gen]]) <- 1:nrow(xinfo[[2]][[n.gen]])
    xinfo[[2]][[n.gen]] <- do.call(rbind, replicate(repx, list(xinfo[[2]][[n.gen]])))
    xinfo[[2]][[n.gen]] <- xinfo[[2]][[n.gen]][order(as.numeric(rownames(xinfo[[2]][[n.gen]]))), , drop=F]
    attr(xinfo[[2]][[n.gen]], "dimnames") <- NULL
  }

  # add selfx to the additional generation.
  if(selfx > 0){
    xinfo[[1]] <- c(xinfo[[1]], replicate(selfx, list(fmat)))
    xinfo[[2]] <- c(xinfo[[2]], replicate(selfx, list(cbind(1:nrow(fmat), 1:nrow(fmat)))))
  }
    
  return(xinfo)
  
}