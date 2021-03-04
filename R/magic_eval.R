#' Main function to design and test MAGIC population.
#'
#' This function takes various input arguments from user and simulates the desired
#' MAGIC design. The MAGIC population can be designed by either providing a pedigree
#' or other input arguments. Depending on the input arguments, a full, partial, basic
#' or deficient MAGIC design can be created. For the partial or deficient designs,
#' the funnels can be generated in either a balanced or unbalanced (random) way.
#' For further information, please refer to the
#' [vignette](https://github.com/cjyang-sruc/magicdesign/magicdesign_vignette.pdf).
#'
#' @param ped a pedigree with 4 columns: individual ID, parent 1 ID, parent 2 ID, generation
#'            number, in the format of either matrix or data.frame.
#' @param n an integer of number of founders.
#' @param m an integer of number of funnels (`balanced=F`) or funnel sets (`balanced=T`).
#' @param reps a vector of replicates in each crossing generation.
#' @param self a vector of number of generations to self after crossing.
#' @param inbred a logical indicator of whether the founders are inbred.
#' @param balanced a logical indicator of whether a balanced partial design is desired.
#' @param minimize a logical indicator of whether to minimize crossing.
#' @param n.try an integer of number of attempts to find balanced partial design (ignored if `balanced=F`).
#' @param addx an integer of either 1 or 2 indicating the type of additional crosses.
#' @param repx an integer of number of replicates in the additional crossing.
#' @param selfx an integer of number of generations to self after additional crossing.
#' @param marker.dist a numerical value of marker distance in Morgan.
#' @param chr.len a vector of chromosome lengths in Morgan.
#' @param n.sim an integer of number of simulations.
#' @param hap.int a numerical value of marker interval for evaluating haplotypes.
#' @param n.hap an integer of 1 or 2 haploid marker data of each RIL are used.
#' @param keep a logical indicator of whether to export the marker data.
#' @return a list of simulation summary.
#'
#' @examples
#' \donttest{
#' mpop <- magic.eval(n=8, m=1, reps=c(1,1,2), self=c(0,0,3), balanced=T)
#' }
#'
#' @export

magic.eval <- function(ped=NULL,
                       n=NULL,
                       m=NULL,
                       reps=NULL,
                       self=NULL,
                       inbred=T,
                       balanced=F,
                       minimize=F,
                       n.try=1000,
                       addx=NULL,
                       repx=1,
                       selfx=3,
                       marker.dist=0.01,
                       chr.len=c(1,2),
                       n.sim=1,
                       hap.int=0.05,
                       n.hap=1,
                       keep=F){

  # argument checks if ped is not provided.
  if(is.null(ped)){
    
    # argument check: n.
    if(n < 3 | !(n%%1==0) | n > 128) stop("n has to be a positive integer between 3 and 128.")
    
    # argument check: m.
    if(m < 0 | !(m%%1==0)) stop("m has to be a non-negative integer.")
    
    # get the number of crossing generations.
    nx <- ceiling(log(n,2))
    
    # argument check: reps.
    if(missing(reps)){
      reps <- rep(1, nx)
      message("argument \"reps\" is missing, defaulting to ", paste(reps, collapse=","), ".")
    }
    if(!is.numeric(reps) | !(length(reps)==nx)) stop("argument \"reps\" has to be a numeric vector of length ", nx, ".")
    if(any(reps < 1) | !all(reps%%1==0)) stop("argument \"reps\" has to be a vector of positive integers.")

    # argument check: self.
    if(missing(self)){
      self <- c(rep(0, nx-1), 3)
      message("argument \"self\" is missing, defaulting to ", paste(self, collapse=","), ".")
    }
    if(!is.numeric(self) | !(length(self)==nx)) stop("argument \"self\ has to be a vector of length ", nx, ".")
    if(any(self < 0) | !all(self%%1==0)) stop("argument \"self\ has to be a vector of non-negative integers.")

    # argument check: balanced.
    if(!is.logical(balanced)) stop("argument \"balanced\" has to be either TRUE (T) or FALSE (F).")

    # argument check: minimize.
    if(!is.logical(minimize)) stop("argument \"minimize\" has to be either TRUE (T) or FALSE (F).")

  }
  
  
  # argument checks regardless of ped is provided or not.
  # argument check: inbred.
  if(!is.logical(inbred)) stop("argument \"inbred\" has to be either TRUE (T) or FALSE (F).")
  
  # argument check: marker.dist.
  if(marker.dist < 0.001) stop("argument \"marker.dist\" has to be 0.001 Morgan or larger.")
  if(marker.dist > 0.1) message("NOTE: argument \"marker.dist\" is given in the unit Morgan.")
  
  # argument check: chr.len.
  if(any(chr.len < marker.dist*2)) stop("argument \"chr.len\" has to have all values equal or larger than marker.dist*2.")
  if(any(chr.len < marker.dist*10)) message("NOTE: at least one chromosome has less than 10 markers.")
  
  # argument check: n.sim.
  if(n.sim < 1 | !(n.sim%%1==0)) stop("argument \"n.sim\" has to be a positive integer.")
  if(n.sim > 1000) message("NOTE: argument \"n.sim\" is large and this will take a while.")
  
  # argument check: hap.int.
  if(hap.int < 0.001) stop("argument \"hap.int\" has to be 0.001 Morgan or larger.")
  if(hap.int > 0.1) message("NOTE: argument \"hap.int\" is given in the unit Morgan.")
  
  # argument check: n.hap.
  if(!any(n.hap==1:2)) stop("argument \"n.hap\" can only take either 1 or 2.")
  
  # argument check: keep
  if(!is.logical(keep)) stop("argument \"keep\" can only be either TRUE (T) or FALSE (F).")

  # set an identifier if ped is provided or not.
  ped.check <- if(is.null(ped)) FALSE else TRUE
    
  # create the founder combination and crossing plan based on input parameters.
  if(is.null(ped)){
    
    if(n == 4){
      
      if(m == 0){
        .n <- n
        xinfo <- magic.basic(n=.n)
      } else if(m == 1){
        .n <- n
        .inbred <- inbred
        xinfo <- magic.full(n=.n, inbred=.inbred)
      }
      
    } else if(n == 8){
      
      if(m == 0){
        .n <- n
        xinfo <- magic.basic(n=.n)
      } else if(m == 45 & balanced){
        .n <- n
        .inbred <- inbred
        xinfo <- magic.full(n=.n, inbred=.inbred)
      } else {
        .n <- n
        .m <- m
        .balanced <- balanced
        .n.try <- n.try
        .inbred <- inbred
        xinfo <- magic.partial(n=.n, m=.m, balanced=.balanced, n.try=.n.try, inbred=.inbred)
      }
      
    } else if(n %in% c(16,32,64,128)){

      if(m == 0){
        .n <- n
        xinfo <- magic.basic(n=.n)
      } else {
        .n <- n
        .m <- m
        .balanced <- balanced
        .inbred <- inbred
        xinfo <- magic.partial(n=.n, m=.m, balanced=.balanced, inbred=.inbred)
      }
      
    } else {

      .n <- n
      .m <- m
      .balanced <- balanced
      .inbred <- inbred
      xinfo <- magic.deficient(n=.n, m=.m, balanced=.balanced, inbred=.inbred)
      
    }
    
    # minimize the crossing if desired, not available for magic.basic.
    if(m > 0 & minimize){
      .xinfo <- xinfo
      xinfo <- magic.minimize(xinfo=.xinfo)
    }
    
    # add the replicates (if any).
    if(any(reps > 1)){
      .xinfo <- xinfo
      .reps <- reps
      xinfo <- magic.reps(xinfo=.xinfo, reps=.reps)
    }
    
    # add selfing (if any).
    .xinfo <- xinfo
    .self <- self
    xinfo <- magic.self(xinfo=.xinfo, self=.self)

    # add additional crossing (if any).
    if(!is.null(addx)){
      .xinfo <- xinfo
      .addx <- addx
      .repx <- repx
      .selfx <- selfx
      xinfo <- magic.addx(xinfo=.xinfo, addx=.addx, repx=.repx, selfx=.selfx)
    }
    
    # convert the design into pedigree so user can check it.
    .xinfo <- xinfo
    ped <- magic.ped(xinfo=.xinfo)
    
    # get the crossing plan for use in simulation.
    xplan <- xinfo[[2]]
    
  } else {
    
    # get the crossing plan from user-supplied pedigree for use in simulation.
    .ped <- ped
    xplan <- magic.ped2cross(ped=.ped)
    class(ped) <- "ped.custom"
    
  }
  
  
  # run the simulations.
  .xplan <- xplan
  .inbred <- inbred
  .marker.dist <- marker.dist
  .chr.len <- chr.len
  .n.sim <- n.sim
  .hap.int <- hap.int
  .n.hap <- n.hap
  .keep <- keep
  out <- magic.sim(xplan=.xplan, inbred=.inbred, marker.dist=.marker.dist, chr.len=.chr.len, n.sim=.n.sim, hap.int=.hap.int, n.hap=.n.hap, keep=.keep)

  # add pedigree to the output
  out <- c(list(ped=ped), out)
  
  # generate attributes for the output.
  info.cross <- vector()
  for(i in 1:length(xplan)){
    if(!all(xplan[[i]][,1] == xplan[[i]][,2])) info.cross <- c(info.cross, nrow(unique(xplan[[i]])))
  }
  
  if(!ped.check){
    
    if(m == 0){
      info.type <- "basic"
    } else if((n == 3 & m == 1) | (n == 4 & m == 1) | (n == 5 & m == 48) | (n == 6 & m == 285) | (n == 7 & m == 135) | (n == 8 & m == 45)){
      info.type <- "full"
    } else {
      info.type <- "partial"
    }
    
    if(info.type == "basic"){
      info.nf <- 1
    } else if(info.type == "full"){
      repx <- 0
      selfx <- 0
      if(n == 3){
        info.nf <- 3
      } else if(n == 4){
        info.nf <- 3
      } else if(n == 5){
        info.nf <- 240
      } else if(n == 6){
        info.nf <- 855
      } else if(n == 7){
        info.nf <- 945
      } else {
        info.nf <- 315
      }
    } else {
      repx <- 0
      selfx <- 0
      if(balanced){
        info.nf <- m*(n-1)
      } else {
        info.nf <- m
      }
    }
    
    attr(out, "info") <- c(n,
                           info.type,
                           paste(c(reps,repx), collapse=","),
                           paste(c(self,selfx), collapse=","),
                           paste(info.cross, collapse=","),
                           length(xplan),
                           nrow(out[[3]][[1]])/n.hap,
                           info.nf)
  } else {
    
    attr(out, "info") <- c(length(unique(c(xplan[[1]]))),
                           "custom",
                           NA,
                           NA,
                           info.cross,
                           length(xplan),
                           nrow(out[[3]][[1]])/n.hap,
                           NA)
    
  }
  
  return(out)
  
}