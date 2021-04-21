#' Convert a pedigree into a crossing plan.
#'
#' This function takes a user provided pedigree and converts it into a crossing plan
#' that can be used in the simulation. The pedigree must have four columns in the
#' following order: individual ID, parent 1 ID, parent 2 ID and generation number.
#' The founders should be present in the pedigree with individual ID of 1 to `n`,
#' parent 1/2 IDs of 0 (or left empty), and generation number of 0.
#'
#' @param ped a pedigree.
#' @return a list of crossing plans for each crossing generation (xplan).
#'
#' @examples
#' \donttest{
#' mped <- cbind(ind=1:13,
#'               p1=c(0,0,0,0,1,1,1,2,2,3,5,6,7),
#'               p2=c(0,0,0,0,2,3,4,3,4,4,10,9,8),
#'               gen=c(0,0,0,0,1,1,1,1,1,1,2,2,2))
#' mpop <- magic.ped2cross(ped=mped)
#' }
#'
#' @export

magic.ped2cross <- function(ped){

  # get the founder information.
  founder <- as.numeric(ped[ped[,4]==0,1])
  if(!(all(founder%in%(1:length(founder))))) stop("Please name the founders sequentially (1 to n).")
  n <- length(founder)
  
  # get the number of generations.
  gen <- as.numeric(unique(ped[,4]))
  gen <- gen[!(gen==0)]
  if(!(all(gen%in%(1:length(gen))))) stop("Please name the generations sequentially (0 to n.gen).")
  n.gen <- length(gen)
  
  # check and make sure all individuals in the first column of ped are unique.
  if(!(length(unique(ped[,1])) == nrow(ped))) stop("The first column of the pedigree should contain unique IDs for each individual.")
  
  # separate the ped into list of ped based on generation.
  ped <- lapply(0:n.gen, FUN=function(x) ped[ped[,4]==x, 1:3, drop=FALSE])
  
  # check if the parents can be found in previous generations.
  for(i in 2:(n.gen+1)){
    if(!all(c(ped[[i]][,2]%in%ped[[i-1]][,1], ped[[i]][,3]%in%ped[[i-1]][,1]))) stop("One or more parent(s) in gen ", i, " is missing from previous generations.")
  }
  
  # convert ped into xplan.
  xplan <- list()
  
  for(i in 2:(n.gen+1)){
    p1 <- sapply(1:nrow(ped[[i]]), FUN=function(x) which(ped[[i]][x,2]==ped[[i-1]][,1]))
    p2 <- sapply(1:nrow(ped[[i]]), FUN=function(x) which(ped[[i]][x,3]==ped[[i-1]][,1]))
    xplan <- c(xplan, list(cbind(p1, p2)))
  }
  
  return(xplan)

}