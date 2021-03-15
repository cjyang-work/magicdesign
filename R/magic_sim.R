#' Simulate populations based on the MAGIC design.
#'
#' This function takes the crossing plans (xplan) and simulates populations based on
#' the crossing plans. Simulation is done using
#' [AlphaSimR](https://cran.r-project.org/web/packages/AlphaSimR/index.html). The founder genotypes in
#' the RILs from each simulation are summarized. By default, the simulated marker data is
#' not kept. If the simulated marker data is really required, please set
#' `n.sim` to 1 or any small value as the output object can be large.
#'
#' @param xplan a list of crossing plans (xplan).
#' @param inbred a logical indicator of whether the founders are inbred.
#' @param marker.dist a numerical value of marker distance in Morgan.
#' @param chr.len a vector of chromosome lengths in Morgan.
#' @param n.sim an integer of number of simulations.
#' @param hap.int a numerical value of marker interval for evaluating haplotypes.
#' @param n.hap an integer of 1 or 2 haploid marker data of each RIL are used.
#' @param keep a logical indicator of whether to keep the marker data.
#' @return a list of simulation summary.
#'
#' @examples
#' \donttest{
#' mpop <- magic.partial(n=8, m=1, balanced=T)
#' mpop <- magic.sim(xplan=mpop$xplan)
#' }
#'
#' @export

magic.sim <- function(xplan, inbred=T, marker.dist=0.01, chr.len=c(1,2), n.sim=1, hap.int=0.05, n.hap=1, keep=F){

  # get the number of generations.
  n.gen <- length(xplan)
  
  # get the number of founders.
  # treat non-inbred founder as 2*n.
  if(inbred){
    n <- length(unique(c(xplan[[1]])))
  } else {
    n <- 2*length(unique(c(xplan[[1]])))
  }
  
  # create founder haplotypes.
  n.chr <- length(chr.len)
  gen.map <- lapply(1:n.chr, FUN=function(x) sort(rep(seq(0, chr.len[x], marker.dist), n-1)))
  gen.map <- c(gen.map, list(c(rep(0, n-1), rep(hap.int, n-1))))
  n.marker <- sapply(1:(n.chr+1), FUN=function(x) length(gen.map[[x]])/(n-1))
  fhap <- lapply(1:(n.chr+1), FUN=function(x) matrix(rep(rbind(0, diag(n-1)), n.marker[x]), nrow=n, ncol=n.marker[x]*(n-1)))
  
  # duplicate the founder haplotype if inbred=T.
  if(inbred){
    for(i in 1:(n.chr+1)){
      rownames(fhap[[i]]) <- 1:nrow(fhap[[i]])
      fhap[[i]] <- rbind(fhap[[i]], fhap[[i]])
      fhap[[i]] <- fhap[[i]][order(as.numeric(rownames(fhap[[i]]))), ]
      rownames(fhap[[i]]) <- NULL
    }
  }

  # temp.rec is a matrix of all possible recombinant haplotypes within hap.int.
  # out.rec is a matrix for storing the count of each recombinant haplotype from each simulation.
  temp.rec <- rbind(sort(rep(1:n,n)),rep(1:n,n))
  temp.rec <- temp.rec[,!(temp.rec[1,]==temp.rec[2,])]
  out.rec <- matrix(0, nrow=ncol(temp.rec), ncol=n.sim)
  row.names(out.rec) <- paste(temp.rec[1,], temp.rec[2,], sep="_")
  
  # out.fprop stores the mean proportion of founder in RILs from each simulation.
  out.fprop <- replicate(n, vector())
  
  # out.fct stores the mean founder count in each chromosome from each simulation.
  out.fct <- replicate(n.chr, vector())
  
  # out.lseg stores the mean non-recombinant segment length in each chromosome from each simulation.
  out.lseg <- replicate(n.chr, vector())
  
  # out.hap collects all the haploid marker data from all simulations, if keep=T.
  if(keep) out.hap <- vector()
  
  # simulate MAGIC population for n.sim-times.
  for(i in 1:n.sim){
    
    # the inbred argument here is different, here we want inbred=F so it will always merge the two haplotypes together.
    founder <- AlphaSimR::newMapPop(genMap=gen.map,
                                    haplotypes=fhap,
                                    inbred=F,
                                    ploidy=2L)
    SP <- AlphaSimR::SimParam$new(founder)
    sim <- AlphaSimR::newPop(founder, simParam=SP)

    for(j in 1:n.gen){
      sim <- AlphaSimR::makeCross(pop=sim,
                                  crossPlan=xplan[[j]],
                                  nProgeny=1,
                                  simParam=SP)
    }

    # extract the haploid marker data for the interval given by hap.int.
    hap.rec <- AlphaSimR::pullSegSiteHaplo(pop=sim, chr=n.chr+1, simParam=SP)
    if(n.hap == 1) hap.rec <- hap.rec[seq(1,nrow(hap.rec),2),,drop=F]
    hap.rec <- magic.hap(hap=hap.rec, n=n)
    
    # extract the haploid marker data for all chromosomes.
    hap.all <- lapply(1:n.chr, FUN=function(x) AlphaSimR::pullSegSiteHaplo(pop=sim, chr=x, simParam=SP))
    if(n.hap == 1) hap.all <- lapply(1:n.chr, FUN=function(x) hap.all[[x]][seq(1,nrow(hap.all[[x]]),2),,drop=F])
    hap.all <- lapply(1:n.chr, FUN=function(x) magic.hap(hap=hap.all[[x]], n=n))
    
    # summarize the proportions of each recombinant haplotype.
    for(j in 1:ncol(temp.rec)){
      out.rec[j,i] <- sum(colSums(t(hap.rec)==temp.rec[,j])==2)/nrow(hap.rec)
    }
    
    # summarize the proportions of founder genome in all chromosomes.
    for(j in 1:n){
      out.fprop[[j]] <- cbind(out.fprop[[j]],
                              rowSums(do.call(cbind, hap.all)==j)/sum(n.marker[1:n.chr]))
    }
    
    # summarize the counts of unique founder in each chromosome.
    for(j in 1:n.chr){
      out.fct[[j]] <- cbind(out.fct[[j]],
                            sapply(1:nrow(hap.all[[j]]), FUN=function(x) length(unique(hap.all[[j]][x,]))))
    }
    
    # summarize the non-recombinant segment length in each chromosome.
    for(j in 1:n.chr){
      temp <- data.frame(founder=c(t(hap.all[[j]])),
                         ril=sort(rep(1:nrow(hap.all[[j]]), ncol(hap.all[[j]]))),
                         pos=rep(seq(0, chr.len[j], marker.dist), nrow(hap.all[[j]])))
      z <- nrow(temp)
      z <- which(temp$founder[-c(1,z)]==temp$founder[-c(z-1,z)] &
                   temp$founder[-c(1,z)]==temp$founder[-c(1,2)] &
                   temp$ril[-c(1,z)]==temp$ril[-c(z-1,z)] &
                   temp$ril[-c(1,z)]==temp$ril[-c(1,2)]) + 1
      temp <- as.matrix(temp[-z,])
      mseg <- vector()
      k <- 1
      while(k < nrow(temp)){
        if(all(temp[k, 1:2] == temp[k+1, 1:2])){
          mseg <- rbind(mseg, c(temp[k, 1:3], temp[k+1, 3]))
          k <- k + 2
        } else {
          mseg <- rbind(mseg, c(temp[k, 1:3], temp[k, 3]))
          k <- k + 1
        }
      }
      if(k == nrow(temp)) mseg <- rbind(mseg, c(temp[k, 1:3], temp[k, 3]))
      
      # shift the start and end position (except the first and last marker) so there is no gap.
      mseg[!(mseg[,3]==0), 3] <- mseg[!(mseg[,3]==0), 3] - marker.dist/2
      mseg[!(mseg[,4]==chr.len[j]), 4] <- mseg[!(mseg[,4]==chr.len[j]), 4] + marker.dist/2
      
      # get the non-recombinant segment length.
      lseg <- mseg[,4] - mseg[,3]
      
      # tabulate the number of segments for each size interval.
      temp <- cbind(seq(0, chr.len[j]-0.01, 0.01) + 1e-6,
                    seq(0.01, chr.len[j], 0.01) + 1e-6)
      lseg <- sapply(1:nrow(temp), FUN=function(x) sum(lseg >= temp[x,1] & lseg < temp[x,2]))
      
      out.lseg[[j]] <- cbind(out.lseg[[j]], lseg/nrow(hap.all[[j]]))
    }
    
    
    # keep the marker data if keep=T.
    if(keep){
      hap.keep <- hap.all
      for(j in 1:n.chr){
        colnames(hap.keep[[j]]) <- paste("chr", j, "_", format(seq(0, chr.len[j], marker.dist), nsmall=3), sep="")
      }
      hap.keep <- do.call(cbind, hap.keep)
      if(!inbred){
        temp <- sort(c(seq(1,n/2,1)+0.1, seq(1,n/2,1)+0.2))
        for(j in 1:n){
          hap.keep[hap.keep==j] <- temp[j]
        }
      }
      rownames(hap.keep) <- paste(i, 1:nrow(hap.keep), sep="_")
      out.hap <- rbind(out.hap, hap.keep)
    }
    
  }
  
  #if(keep){
  #  temp <- paste("sim_ril_fhap_", gsub(" ", "_", gsub("-", "", gsub(":", "", Sys.time()))), ".csv", sep="")
  #  write.csv(out.hap, temp, quote=F, row.names=T)
  #}
  
  if(keep){
    out <- list(rec=out.rec, fprop=out.fprop, fct=out.fct, lseg=out.lseg, hap=out.hap)
  } else {
    out <- list(rec=out.rec, fprop=out.fprop, fct=out.fct, lseg=out.lseg)
  }

  return(out)

}