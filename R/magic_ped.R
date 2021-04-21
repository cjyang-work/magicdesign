#' Convert a "cross.info" object into a pedigree.
#'
#' This function takes the "cross.info" object and converts it into a pedigree that
#' has 4 columns: individual ID, parent 1 ID, parent 2 ID and generation number.
#'
#' @param xinfo an object of "cross.info" class.
#' @return a pedigree.
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=TRUE)
#' mped <- magic.ped(xinfo=mpop)
#' }
#'
#' @export

magic.ped <- function(xinfo){

  # check if xinfo is an object of "cross.info" class.
  if(!methods::is(xinfo, "cross.info")) stop("xinfo has to be an object of \"cross.info\" class.")
  
  # get the number of founders.
  n <- length(unique(c(xinfo[[1]][[1]])))
  
  # extract fcomb and xplan from xinfo, and add the founders to fcomb.
  fcomb <- c(list(matrix(1:n, ncol=1)), xinfo[[1]])
  xplan <- xinfo[[2]]
  n.gen <- length(xinfo[[1]])

  # add indices to indicate siblings, if any.
  idx <- lapply(1:n.gen, FUN=function(x) rep(0, nrow(xplan[[x]])))
  for(i in 1:n.gen){
    for(j in nrow(xplan[[i]]):1){
      if(sum(colSums(t(xplan[[i]])==xplan[[i]][j,])==2) > 1){
        idx[[i]][j] <- sum(colSums(t(xplan[[i]][1:j, , drop=FALSE])==xplan[[i]][j,])==2)
      }
    }
  }
  temp <- nchar(max(unlist(idx)))
  idx <- lapply(1:n.gen, FUN=function(x) formatC(idx[[x]], width=temp, flag="0"))
  
  # identify which generation has selfing and maintain the indices for replicates in the selfed individuals.
  temp <- sapply(1:n.gen, FUN=function(x) all(xplan[[x]][,1] == xplan[[x]][,2]))
  
  # create the replicate identifier at each crossing generations - this carries over from the first to last cross.
  x.gen <- which(!temp)
  z <- c(list(idx[x.gen[1]]), replicate(length(x.gen)-1, list()))
  for(i in 2:length(x.gen)){
    for(j in 1:i){
      if(j < i){
        z[[i]][[j]] <- paste(z[[i-1]][[j]][xplan[[x.gen[i]]][,1]], z[[i-1]][[j]][xplan[[x.gen[i]]][,2]], sep="")
      } else {
        z[[i]][[j]] <- idx[[x.gen[i]]]
      }
    }
  }
  
  # add letter to indicate the crossing generation at which the replicates happen.
  for(i in 1:length(z)){
    for(j in 1:length(z[[i]])){
      z[[i]][[j]] <- paste(LETTERS[j], z[[i]][[j]], sep="")
    }
  }
  
  # merge all the replicate information together and add them back to idx.
  for(i in 1:length(x.gen)) idx[[x.gen[i]]] <- do.call(paste, c(z[[i]], sep=""))
  
  # add selfing information.
  k <- 1
  for(i in which(temp)){
    if(temp[i-1]){
      k <- k + 1
      idx[[i]] <- gsub("s.*", paste("s", k, sep=""), idx[[i-1]])
    } else {
      k <- 1
      idx[[i]] <- paste(idx[[i-1]], "s", k, sep="")
    }
  }
  
  idx <- c(list(rep("a", n)), idx)
  
  # get the replicate/selfing information for the parents as well.
  p1 <- p2 <- replicate(n.gen, vector())
  id1 <- id2 <- replicate(n.gen, vector())
  
  for(i in 1:n.gen){
    p1[[i]] <- fcomb[[i]][xplan[[i]][,1],,drop=FALSE]
    id1[[i]] <- idx[[i]][xplan[[i]][,1]]
    p2[[i]] <- fcomb[[i]][xplan[[i]][,2],,drop=FALSE]
    id2[[i]] <- idx[[i]][xplan[[i]][,2]]
  }
  
  fcomb <- fcomb[-1]
  idx <- idx[-1]
  
  # combine the individual id, selfing identifier and sibling indices.
  for(i in 1:n.gen){
    fcomb[[i]] <- do.call(paste, c(data.frame(fcomb[[i]]), sep=";"))
    p1[[i]] <- do.call(paste, c(data.frame(p1[[i]]), sep=";"))
    p2[[i]] <- do.call(paste, c(data.frame(p2[[i]]), sep=";"))
  }
  
  for(i in 1:n.gen){
    fcomb[[i]] <- paste(fcomb[[i]], idx[[i]], sep="_")
    p1[[i]] <- paste(p1[[i]], id1[[i]], sep="_")
    p2[[i]] <- paste(p2[[i]], id2[[i]], sep="_")
  }
  
  # add a column of generation.
  gen <- lapply(1:n.gen, FUN=function(x) rep(x, length(fcomb[[x]])))
  
  # create the pedigree.
  out <- cbind(unlist(fcomb), unlist(p1), unlist(p2), unlist(gen))
  out <- gsub("_a", "", out)
  out <- rbind(cbind(1:n,"","",0), out)
  
  # set the attributes.
  if(log(n,2)%%1==0) attr(out, "complete") <- TRUE
  
  return(out)
}