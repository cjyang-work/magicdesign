#' Plot pedigree.
#'
#' This function plots the pedigree based on the type of design.
#' By default, the function will plot a comprehensive pedigree for
#' `n` of 4, 8, 16, 32, 64, 128 and a simple pedigree for other `n`.
#' The pedigree is generated from either [magic.ped] or [magic.eval].
#' Note: if the MAGIC design is created from a user-provided pedigree,
#' then the function will always plot a simple pedigree.
#'
#' @param ped a pedigree.
#' @param basic a logical indicator of whether it is a basic design or not.
#' @param show.partial a logical indicator of whether to plot a partial pedigree or not.
#' @param w2h.ratio a numerical value of width-to-height-ratio in the pedigree plot.
#' @param force.option a logical indicator of whether to force the function to plot
#'        using a simpler option.
#' @return a HTML file of an interactive pedigree plot.
#'
#' @examples
#' \donttest{
#' mpop <- magic.eval(n=8, m=1, reps=c(1,1,2), self=c(0,0,3), balanced=TRUE)
#' magic.ped2plot(ped=mpop$ped)
#' }
#'
#' @export

magic.ped2plot <- function(ped, basic=FALSE, show.partial=FALSE, w2h.ratio=2, force.option=FALSE){

  # if basic=TRUE, show.partial cannot be TRUE.
  if(basic & show.partial) stop("show.partial has to be FALSE if basic is set to TRUE.")

  # use the more complete pedigree plotting option for pedigree generated from this tool (only available for n = 4,8,16,32,64,128).
  # for the others (e.g. user input pedigree, or other n), use the simpler pedigree plotting option.
  # if there is a bug with the more complete pedigree plotting option, setting force.option=TRUE will plot using the simpler option.
  if(!is.null(attr(ped, "complete")) & !force.option){
    .ped <- ped
    .basic <- basic
    .show.partial <- show.partial
    .w2h.ratio <- w2h.ratio
    magic.ped4perfect(ped=.ped, basic=.basic, show.partial=.show.partial, w2h.ratio=.w2h.ratio)
  } else {
    .ped <- ped
    .w2h.ratio <- w2h.ratio
    magic.ped4NP2(ped=.ped, w2h.ratio=.w2h.ratio)
  }
    
}