#' Create a partial MAGIC design.
#'
#' This function produces m (or m-set) founder combinations and crosses for
#' 8 or more founders.
#'
#' @param n number of founders.
#' @param m number of funnel sets (balanced=T) or funnels (balanced=F).
#' @param balanced logical indicator of whether balanced partial design is chosen or ignored.
#' @param n.try number of attempts to find balanced partial design (default to 1000, applicable only if n=8 and balanced=T).
#' @param inbred logical indicator of whether the founders are inbred (default to TRUE).
#' @return an object of "cross.info" class, which is a list of
#'         founder combinations (fcomb) and crossing plans (xplan).
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=T)
#' }
#'
#' @export

magic.partial <- function(n, m, balanced, n.try=1000, inbred=T){
  
  # check if n is within allowed values.
  if(!(n %in% c(8,16,32,64,128))) stop("n has to be a power of 2 and cannot be smaller than 8. e.g. 8, 16, 32, 64, 128.")
  
  # check if m is a positive integer.
  if(m < 1 | !(m%%1==0)) stop("m has to be a positive integer.")
  
  # check if balanced is a logical indicator (T/F).
  if(!is.logical(balanced)) stop("balanced has to be either TRUE (T) or FALSE (F)")
  
  # get the number of crossing generations.
  nx <- log(n, 2)
  
  # check if n is a power of 2.
  if(!(nx%%1==0)) stop("n has to be a power of 2. e.g. 8, 16, 32.")
  
  # find partial design for 8 founders.
  if(n == 8){
  
    # get fmat for n=8.
    .n <- n
    .inbred <- inbred
    fmat <- magic.full(n=.n, inbred=.inbred)[[1]][[nx]]

    # find balanced partial design for 8 founders.
    if(balanced){

      # check if n.try is a positive integer.
      if(n.try < 1 | !(n.try%%1==0)) stop("n.try has to be a positive integer")
      
      # get db8 that is previously generated using magic.db().
      db <- db8
      db <- lapply(1:45, FUN=function(x) db[(16*(x-1)+1):(16*x),])

      # total number of funnels in 8 founders MAGIC is 315 = 45*m.
      # so, the accepted values of m range from 1 to 44, with 45 being full design.
      # if m is larger than 22, then we will switch to subtraction method (faster).
      if(m < 1 | m > 44){
        stop("m is out of range (1 to 44).")
      } else if(m > 22 & m < 45){
        mm <- 45 - m
      } else {
        mm <- m
      }

      # find m-partial designs by randomly sampling 45 sets of funnel and removing any set with redundant funnels.
      for(i in 1:n.try){

        # random sampling of db8.
        idx1 <- sample(1:45, 45, replace=F)
        idx2 <- sample(1:16, 45, replace=T)
        
        # merge all randomly sampled subsets.
        temp <- sapply(1:45, FUN=function(x) db[[idx1[x]]][idx2[x],])
        
        # loop to identify the largest, balanced and non-duplicated subsets of MAGIC design.
        while(ncol(temp) > 1){
          temp.ct <- sapply(1:ncol(temp), FUN=function(x) sum(c(temp[,-x])%in%temp[,x]))
          if(any(temp.ct > 0)){
            temp <- temp[, -which.max(temp.ct)]
          } else {
            break
          }
        }
        
        # check if the identified designs fit within the desired m-sets.
        # if yes, then exit the loop, otherwise continue with random search until n.try is done.
        if(ncol(temp) >= mm){
          temp <- sort(c(temp[, 1:mm]))
          break
        }
        
      }
      
      # convert the indices if subtraction method is used.
      idx <- if(m>22) c(1:315)[-temp] else temp
      
    } else {
      
      # find partial design for 8 founders while ignoring balanced design.
      idx <- sort(sample(1:nrow(fmat), m, replace=F))
      
    }
    
    # extract the founder combinations for partial design.
    fmat <- fmat[idx, , drop=F]
    

    # find partial design for more than 8 founders.
  } else if(n > 8){
    
    # find balanced partial design for more than 8 founders.
    if(balanced){
      
      # no db is available for n > 8, so we use blocksdesign package.
      # get balanced partial design from blocksdesign.
      # blocksdesign can result in redundant/replicated combinations for large m with 8 founders.
      # since we only use blocksdesign for n > 8, this should be OK.
      .fmat <- blocksdesign::blocks(treatments=n, replicates=(n-1)*m, blocks=c(list((n-1)*m),replicate(nx-1,list(2))))$Plan
      .fmat <- t(matrix(as.numeric(t(as.matrix(.fmat[,(ncol(.fmat)-1):ncol(.fmat)]))), nrow=n))
      fmat <- magic.rearrange(fmat=.fmat)
      
    } else {
      
      # find partial design for more than 8 founders while ignoring balanced design.
      # there is a loop to replace redundant/replicated combinations with non-redundant combinations.
      fmat <- vector()
      k <- 0
      z <- 1
      
      while(k < m){
        
        # randomly generate one funnel.
        temp <- magic.rearrange(matrix(sample(1:n, n, replace=F), nrow=1))
        temp <- rbind(fmat, temp)
        
        # search for a new funnel if the funnel is not unique (limited to 100 tries).
        if(nrow(temp)==nrow(unique(temp))){
          fmat <- temp
          k <- k + 1
          z <- 1
        } else {
          z <- z + 1
          if(z > 100){
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