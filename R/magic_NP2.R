#' Create a Non-Power of 2 (NP2) MAGIC design.
#'
#' This function produces `m` (or `m`-set) founder combinations and crosses for
#' number of founders that is not a power of 2 (e.g. 3,5,6,7,9,...).
#' For `balanced=TRUE`, `n` is restricted to 3, 5-7, 9-15. For `balanced=FALSE`,
#' `n` is limited to 3, 5-7, 9-15, 17-31, 33-63, 65-127.
#' Please refer to the section on NP2 MAGIC design in the
#' [vignette](https://cjyang-sruc.github.io/files/magicdesign_vignette)
#' for more information on the accepted `m` values for each `n`.
#'
#' @param n number of founders.
#' @param m number of funnel sets (`balanced=TRUE`) or funnels (`balanced=FALSE`).
#' @param balanced logical indicator of whether balanced partial design is chosen or ignored.
#' @param inbred logical indicator of whether the founders are inbred.
#' @return an object of "cross.info" class, *i.e.* a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.NP2(n=5, m=2, balanced=TRUE)
#' }
#'
#' @export

magic.NP2 <- function(n, m, balanced=FALSE, inbred=TRUE){
  
  # check to make sure that n is larger than 2 and not a power of 2.
  if(n < 3 | log(n,2)%%1==0 | n > 128) stop("n has to be between 3 to 127 and not a power of 2.")

  # argument check: m.
  if(!(m%%1==0)) stop("m has to be an integer.")
  
  # argument check: ranges of m.
  m.check <- cbind(c(3, 5:7, 9:15, 17:31, 33:63, 65:127),
                   c(1, rep(1,3), rep(1,7), rep(1,15), rep(1,31), rep(1,63)),
                   c(1, 48, 285, 135, rep(100, 116)),
                   c(3, 240, 855, 945, rep(10000, 116)))
  if(balanced & (m < m.check[m.check[,1]==n, 2] | m > m.check[m.check[,1]==n, 3])){
    stop("invalid m for the selected n.")
  } else if(!balanced & (m < m.check[m.check[,1]==n, 2] | m > m.check[m.check[,1]==n, 4])){
    stop("invalid m for the selected n.")
  }
  
  # argument check: balanced.
  if(!is.logical(balanced)) stop("argument \"balanced\" has to be either TRUE (T) or FALSE (F).")
  
  # argument check: inbred.
  if(!is.logical(inbred)) stop("argument \"inbred\" has to be either TRUE (T) or FALSE (F).")


  # find the number of founders needed in first cross (e.g. n=5 means 8 founders, n=13 means 16 founders).
  n0 <- 2^ceiling(log(n,2))
  
  # get the number of crossing generations.
  nx <- log(n0, 2)
  
  # full design is only available for n=3,5,6,7.
  # full design requires m=1,48,285,135 for n=3,5,6,7 respectively.
  # for partial designs, if balanced=TRUE, m indicates number of semi-balanced funnel sets.
  # for partial designs, if balanced=FALSE, m indicates number of funnels.
  if(balanced){
    
    if(n==3 & m==1){
      fmat <- db3
    } else if(n==5 & m==48){
      fmat <- db5
    } else if(n==6 & m==285){
      fmat <- db6
    } else if(n==7 & m==135){
      fmat <- db7
    } else {
      
      if(n < 16){
        
        # get the number of founder occurrence (nfo) and number of funnel (nf) for use in blocksdesign.
        nf <- 1
        while(!((nf*n0/n)%%1 == 0)){
          nf <- nf + 1
        }
        nfo <- nf*n0/n
        
        # warns user that large m may take a while, sometimes not possible to complete.
        if(nfo*m > 200) message("this may take a while and sometimes not possible to complete; try reducing m if this fails.")
        
        # find semi-balanced partial design using the blocksdesign package.
        # it won't be fully balanced - we can maintain equal representation of founders in the final RILs,
        # but unlikely to get equal representation of founder crosses.
        # at best, the founder crosses will be approximately balanced as m increases.
        .fmat <- blocksdesign::blocks(treatments=n, replicates=nfo*m, blocks=c(list(nf*m),replicate(nx-1,list(2))))$Plan
        .fmat <- t(matrix(as.numeric(t(as.matrix(.fmat[,(ncol(.fmat)-1):ncol(.fmat)]))), nrow=n0))
        fmat <- magic.rearrange(fmat=.fmat)
        
      } else {
        
        stop("Balanced partial design is not available for n > 16.")
        
      }
      
    }
      
  } else {
    
    # for partial unbalanced design, we randomly sample the combinations.
    # for n < 8 we have db to use, otherwise we will randomly search for the combinations.
    # here, m is the number of funnels, NOT the number of funnel sets.
    
    if(n < 8){
      
      if(n==3){
        fmat <- db3[sample(1:nrow(db3), m), ]
      } else if(n==5){
        fmat <- db5[sample(1:nrow(db5), m), ]
      } else if(n==6){
        fmat <- db6[sample(1:nrow(db6), m), ]
      } else if(n==7){
        fmat <- db7[sample(1:nrow(db7), m), ]
      }
      
    } else {
      
      # k ensures the loop ends when m-funnels have been identified.
      # z limits the search for unique k-funnel to 100 times.
      fmat <- vector()
      k <- 0
      z <- 1
      
      while(k < m){
        
        # randomly generate one funnel.
        temp <- c(1:n, sample(1:n, n0-n, replace=FALSE))
        temp <- sample(temp, n0, replace=FALSE)
        temp <- magic.rearrange(fmat=matrix(temp,nrow=1))
        
        # check if the funnel involves crosses of two same individuals, e.g. 1;2 x 1;2.
        b <- vector()
        for(i in 1:nx){
          a <- matrix(temp, nrow=2^(nx-i+1))
          b <- c(b, any(rowSums(a[seq(1,nrow(a),2),,drop=FALSE] == a[seq(2,nrow(a),2),,drop=FALSE]) == 2^(i-1)))
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
