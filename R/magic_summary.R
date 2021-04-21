#' Tabulate the comparisons of different MAGIC designs.
#'
#' This function takes a list of outputs from [magic.eval] and compares the MAGIC designs.
#' It is similar to [magic.plot], excepts the results are shown in tables instead of figures.
#'
#' @param input a list of outputs from [magic.eval].
#' @param complete a logical indicator of whether to produce a complete summary or not.
#' @param design.names a vector of design names (optional).
#' @return a list of 6 different comparisons.
#'
#' @seealso [magic.plot]
#'
#' @examples
#' \donttest{
#' mpop1 <- magic.eval(n=8, m=1, reps=c(1,1,4), self=c(0,0,3), balanced=TRUE, n.sim=10)
#' mpop2 <- magic.eval(n=8, m=7, reps=c(1,1,4), self=c(0,0,3), balanced=FALSE, n.sim=10)
#' mpop <- magic.summary(input=list(mpop1, mpop2))
#' }
#'
#' @export


magic.summary <- function(input, complete=FALSE, design.names=NULL){

  # get some information from the input
  n.design <- length(input)
  
  if(is.null(design.names)){
    design.names <- 1:n.design
  } else {
    if(!(length(design.names)==n.design)) stop("design.names has to be a vector of length equals to number of designs.")
  }
  
  n.sim <- unique(sapply(1:n.design, FUN=function(x) ncol(input[[x]][[2]])))
  if(length(n.sim) > 1) stop("all designs must have the same number of simulations.")
  
  n <- unique(sapply(1:n.design, FUN=function(x) as.numeric(attr(input[[x]], "info")[1])))
  if(length(n) > 1) stop("all designs must have the same number of founders.")
  
  n.chr <- unique(sapply(1:n.design, FUN=function(x) length(input[[x]][[4]])))
  if(length(n.chr) > 1) stop("all designs must have the same number of chromosomes.")
  
  # do not proceed if the number of RILs is less than 10.
  temp <- sapply(1:n.design, FUN=function(x) as.numeric(attr(input[[x]], "info")[7]) < 10)
  if(any(temp)) stop("one or more designs have too few RILs (< 10), please increase the population size.")
  
  # get the information about all the designs under comparison.
  dat <- sapply(1:n.design, FUN=function(x) attr(input[[x]], "info"))
  dat <- data.frame(dat, stringsAsFactors=FALSE)
  rownames(dat) <- c("founder", "type", "reps", "self", "cross", "generation", "RIL", "funnel")
  colnames(dat) <- paste("design ", design.names, sep="")


  if(complete){
    
    # out1: proportions of all recombinant haplotypes.
    out1 <- data.frame(mean=sapply(1:n.design, FUN=function(x) mean(colSums(input[[x]][[2]]))),
                       variance=sapply(1:n.design, FUN=function(x) stats::var(colSums(input[[x]][[2]]))))
    rownames(out1) <- paste("design ", design.names, sep="")

    # out2: number of unique recombinant haplotypes.
    out2 <- data.frame(mean=sapply(1:n.design, FUN=function(x) mean(colSums(!(input[[x]][[2]]==0)))),
                       variance=sapply(1:n.design, FUN=function(x) stats::var(colSums(!(input[[x]][[2]]==0)))))
    rownames(out2) <- paste("design ", design.names, sep="")
    
    # out3: proportions of individual recombinant haplotypes.
    out3 <- vector()
    for(i in 1:n.design){
      out3 <- cbind(out3, sapply(1:nrow(input[[i]][[2]]), FUN=function(x) mean(input[[i]][[2]][x,])))
    }
    
    for(i in 1:n.design){
      out3 <- cbind(out3, sapply(1:nrow(input[[i]][[2]]), FUN=function(x) stats::var(input[[i]][[2]][x,])))
    }
    
    out3 <- data.frame(out3)
    colnames(out3) <- c(paste("mean.", design.names, sep=""), paste("var.", design.names, sep=""))
    rownames(out3) <- rownames(input[[1]][[2]])
    
    # out4: proportion of founder genome in the final MAGIC population.
    temp <- list()
    for(i in 1:n.design){
      temp[[i]] <- lapply(1:n, FUN=function(x) colSums(input[[i]][[3]][[x]])/nrow(input[[i]][[3]][[x]]))
    }
    
    out4 <- vector()
    for(i in 1:n.design){
      out4 <- cbind(out4, sapply(1:n, FUN=function(x) mean(temp[[i]][[x]])))
    }
    
    for(i in 1:n.design){
      out4 <- cbind(out4, sapply(1:n, FUN=function(x) stats::var(temp[[i]][[x]])))
    }
    
    out4 <- data.frame(out4)
    colnames(out4) <- c(paste("mean.", design.names, sep=""), paste("var.", design.names, sep=""))
    rownames(out4) <- paste("founder", 1:n, sep=" ")
    
    # out5: proportions of 1 to n unique founder alleles in the final population (for each chromosome).
    temp <- replicate(n.design, list(replicate(n.chr+1, vector())))
    for(i in 1:n.design){
      for(j in 1:n.sim){
        temp[[i]][[n.chr+1]] <- cbind(temp[[i]][[n.chr+1]], rowSums(sapply(1:n, FUN=function(x) input[[i]][[3]][[x]][,j]) > 0))
      }
    }
    
    for(i in 1:n.design){
      for(j in 1:n.chr){
        temp[[i]][[j]] <- input[[i]][[4]][[j]]
      }
    }
    
    for(i in 1:n.design){
      for(j in 1:(n.chr+1)){
        temp[[i]][[j]] <- sapply(1:n, FUN=function(x) colSums(temp[[i]][[j]]==x)/nrow(temp[[i]][[j]]))
        if(n.sim == 1) temp[[i]][[j]] <- t(temp[[i]][[j]])
      }
    }
    
    out5 <- replicate(n.chr+1, vector())
    for(i in 1:(n.chr+1)){
      for(j in 1:n.design){
        out5[[i]] <- cbind(out5[[i]], sapply(1:n, FUN=function(x) mean(temp[[j]][[i]][, x])))
      }
    }
    
    for(i in 1:(n.chr+1)){
      for(j in 1:n.design){
        out5[[i]] <- cbind(out5[[i]], sapply(1:n, FUN=function(x) stats::var(temp[[j]][[i]][, x])))
      }
    }
    
    for(i in 1:(n.chr+1)){
      out5[[i]] <- data.frame(out5[[i]])
      colnames(out5[[i]]) <- c(paste("mean.", design.names, sep=""), paste("var.", design.names, sep=""))
      rownames(out5[[i]]) <- paste(1:n, "unique founder", sep=" ")
    }
    
    names(out5) <- c(paste("chr", 1:n.chr, sep=" "), "all")
    
    # out6: mean count of different non-recombinant segment lengths in each design.
    out6 <- replicate(n.chr, vector())
    for(i in 1:n.chr){
      for(j in 1:n.design){
        out6[[i]] <- cbind(out6[[i]], sapply(1:nrow(input[[j]][[5]][[i]]), FUN=function(x) mean(input[[j]][[5]][[i]][x, ])))
      }
    }
    
    for(i in 1:n.chr){
      for(j in 1:n.design){
        out6[[i]] <- cbind(out6[[i]], sapply(1:nrow(input[[j]][[5]][[i]]), FUN=function(x) stats::var(input[[j]][[5]][[i]][x, ])))
      }
    }
    
    for(i in 1:n.chr){
      out6[[i]] <- data.frame(out6[[i]])
      colnames(out6[[i]]) <- c(paste("mean.", design.names, sep=""), paste("var.", design.names, sep=""))
      rownames(out6[[i]]) <- paste(seq(0, nrow(out6[[i]])-1, 1),
                                   "-",
                                   seq(1, nrow(out6[[i]]), 1),
                                   " cM",
                                   sep="")
    }
    
    out <- list(summary=dat,
                prop.all.rec.hap=out1,
                num.unique.hap=out2,
                prop.ind.rec.hap=out3,
                prop.f.genome=out4,
                prop.n.unique.f=out5,
                non.rec.seg.len=out6)
    
  } else {
    
    out <- dat
    
  }
  
  return(out)
  
}