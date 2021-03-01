#' Create a deficient MAGIC design.
#'
#' This function produces m (or m-set) founder combinations and crosses for
#' number of founders that is not a power of 2 (e.g. 3,5,6,7,9,...).
#'
#' @param n number of founders.
#' @param m number of funnel sets (balanced=T) or funnels (balanced=F).
#' @param balanced logical indicator of whether balanced partial design is chosen or ignored.
#' @param inbred logical indicator of whether the founders are inbred (default to TRUE).
#' @return an object of "cross.info" class, which is a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.deficient(n=5, m=2, balanced=T)
#' }
#'
#' @export

magic.deficient <- function(n, m, balanced, inbred=T){
  
  # check to make sure that n is larger than 2 and not a power of 2.
  if(n < 3 | log(n,2)%%1==0) stop("n has to be larger than 2 and not a power of 2.")

  # find the number of founders needed in first cross (e.g. n=5 means 8 founders, n=13 means 16 founders).
  n0 <- 2^ceiling(log(n,2))
  
  # get the number of crossing generations.
  nx <- log(n0, 2)
  
  # full design is only available for n=3,5,6,7 (ignores "balanced" argument).
  # full design requires m=1,48,285,135 for n=3,5,6,7 respectively.
  # for partial designs, if balanced=T, m indicates number of semi-balanced funnel sets.
  # for partial designs, if balanced=F, m indicates number of funnels.
  if(((n==3 & m==1) | (n==5 & m==48) | (n==6 & m==285) | (n==7 & m==135)) & balanced){
    
    # use the permutations function from gtools to generate all possible permutations.
    fmat <- gtools::permutations(n=n0, r=n0, v=1:n0)
    temp <- t(combn(x=1:n, m=n0-n))
    fmat2 <- vector()

    # remove redundant combinations (e.g. 1231 = 2131) and same individual crosses (e.g. 1123).
    for(i in 1:nrow(temp)){
      fmat1 <- fmat
      for(j in 1:ncol(temp)) fmat1[fmat1==(n+j)] <- temp[i,j]
      fmat1 <- unique(magic.rearrange(fmat=fmat1))
      b <- vector()
      for(j in 1:(nx-1)){
        a <- lapply(seq(1,n0,2^j), FUN=function(x) fmat1[, x:(x+2^j-1), drop=F])
        a <- do.call(rbind, a)
        a <- rowSums(a[, 1:(ncol(a)/2), drop=F]==a[, (ncol(a)/2+1):ncol(a), drop=F]) == (ncol(a)/2)
        a <- rowSums(matrix(a, ncol=2^(nx-j))) > 0
        b <- cbind(b, a)
      }
      fmat1 <- fmat1[rowSums(b) == 0, , drop=F]
      fmat2 <- rbind(fmat2, fmat1)
    }
    fmat <- fmat2
    
  } else {
    
    if(balanced){
      
      # get the number of founder occurrence (nfo) and number of funnel (nf) for use in blocksdesign.
      nf <- 1
      while(!((nf*n0/n)%%1 == 0)){
        nf <- nf + 1
      }
      nfo <- nf*n0/n

      # find semi-balanced partial design using the blocksdesign package.
      # it won't be fully balanced - we can maintain equal representation of founders in the final RILs,
      # but unlikely to get equal representation of founder crosses.
      # at best, the founder crosses will be approximately balanced as m increases.
      .fmat <- blocksdesign::blocks(treatments=n, replicates=nfo*m, blocks=c(list(nf*m),replicate(nx-1,list(2))))$Plan
      .fmat <- t(matrix(as.numeric(t(as.matrix(.fmat[,(ncol(.fmat)-1):ncol(.fmat)]))), nrow=n0))
      fmat <- magic.rearrange(fmat=.fmat)
      
    } else {
      
      # for unbalanced partial design, we randomly sample the combinations.
      # here, m is the number of funnels, NOT the number of funnel sets.
      # k ensures the loop ends when m-funnels have been identified.
      # z limits the search for unique k-funnel to 100 times.
      fmat <- vector()
      k <- 0
      z <- 1
      
      while(k < m){
        
        # randomly generate one funnel.
        temp <- c(1:n, sample(1:n, n0-n, replace=F))
        temp <- sample(temp, n0, replace=F)
        temp <- magic.rearrange(fmat=matrix(temp,nrow=1))
        
        # check if the funnel involves crosses of two same individuals, e.g. 1;2 x 1;2.
        b <- vector()
        for(i in 1:nx){
          a <- matrix(temp, nrow=2^(nx-i+1))
          b <- c(b, any(rowSums(a[seq(1,nrow(a),2),,drop=F] == a[seq(2,nrow(a),2),,drop=F]) == 2^(i-1)))
        }
        temp <- rbind(fmat, temp)
        
        # search for a new funnel if there is any cross of two same individuals within the funnel.
        # search also for a new funnel if the funnel is not unique (limited to 100 tries).
        if(!any(b) & nrow(temp)==nrow(unique(temp))){
          fmat <- temp
          k <- k + 1
          z <- 1
        } else {
          z <- z + 1
          if(!any(b) & z > 100){
            fmat <- temp
            k <- k + 1
            z <- 1
          }
        }
        
      }
      
    }
  }

  
  # now we have the final founder combinations, we back-calculate the crosses for earlier generations.
  fcomb <- replicate(nx, list())
  xplan <- replicate(nx, list())
  
  fcomb[[nx]] <- fmat
  
  for(i in (nx-1):1){
    fcomb[[i]] <- rbind(fcomb[[i+1]][,1:(ncol(fcomb[[i+1]])/2)],
                        fcomb[[i+1]][,(ncol(fcomb[[i+1]])/2 + 1):ncol(fcomb[[i+1]])])
  }
  
  # remove unnecessary 2-ways if the founders are inbred.
  if(inbred){
    fcomb[[1]] <- unique(fcomb[[1]])
  } else {
    xplan[[2]] <- matrix(1:nrow(fcomb[[1]]), ncol=2)
  }
  
  xplan[[1]] <- fcomb[[1]]
  
  for(i in 3:nx){
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
