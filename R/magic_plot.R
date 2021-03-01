#' Plot and compare different MAGIC designs.
#'
#' This function takes a list of outputs from magic.eval() and compares the MAGIC designs
#' via various plots. There are two plot display option available: "interval" highlights
#' the recombinations within the previously specified hap.int argument, while "whole"
#' shows the distribution of founder genomes in the RILs.
#'
#' @param input a list of outputs from magic.eval().
#' @param display a character indicator of whether "interval" or "whole" plot display is desired.
#' @param fpair a matrix of two columns of founder pairs (ignored if display="whole").
#' @param chr.names a vector of chromosome names (optional).
#' @return multi-section plots.
#'
#' @seealso [magic.summary]
#'
#' @examples
#' \donttest{
#' mpop1 <- magic.eval(n=8, m=1, reps=c(1,1,4), self=c(0,0,3), balanced=T, n.sim=10)
#' mpop2 <- magic.eval(n=8, m=7, reps=c(1,1,4), self=c(0,0,3), balanced=F, n.sim=10)
#' magic.plot(input=list(mpop1, mpop2), display="interval")
#' }
#'
#' @export

magic.plot <- function(input,
                       display=c("interval", "whole"),
                       fpair=NULL,
                       chr.names=NULL){
  
  # get some information from the input
  n.design <- length(input)
  
  n.sim <- unique(sapply(1:n.design, FUN=function(x) ncol(input[[x]][[2]])))
  if(length(n.sim) > 1) stop("all designs must have the same number of simulations.")
  
  n <- unique(sapply(1:n.design, FUN=function(x) as.numeric(attr(input[[x]], "info")[1])))
  if(length(n) > 1) stop("all designs must have the same number of founders.")
  
  n.comb <- choose(n,2)
  
  n.chr <- unique(sapply(1:n.design, FUN=function(x) length(input[[x]][[4]])))
  if(length(n.chr) > 1) stop("all designs must have the same number of chromosomes.")
  
  # check which display is desired.
  display <- match.arg(display)
  
  # do not proceed if the number of RILs is less than 10.
  temp <- sapply(1:n.design, FUN=function(x) as.numeric(attr(input[[x]], "info")[7]) < 10)
  if(any(temp)) stop("one or more designs have too few RILs (< 10), please increase the population size.")

  # display=interval
  if(display=="interval"){
    
    # plot1: proportions of all recombinant haplotypes.
    dat1 <- lapply(1:n.design, FUN=function(x) data.frame(design=x, rec=colSums(input[[x]][[2]]), stringsAsFactors=F))
    dat1 <- do.call(rbind, dat1)
    dat1$design <- as.factor(dat1$design)
    plot1 <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data=dat1, ggplot2::aes(x=design, y=rec, fill=design), alpha=0.5, lwd=0.2, outlier.shape=NA) +
      ggplot2::geom_jitter(data=dat1, ggplot2::aes(x=design, y=rec), shape=16, alpha=0.3, width=0.25, height=0) +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      ggplot2::ylab("prop. rec.") +
      ggplot2::labs(tag="A")
    
    # plot2: number of unique recombinant haplotypes.
    dat2 <- lapply(1:n.design, FUN=function(x) data.frame(design=x, rec=colSums(!(input[[x]][[2]]==0)), stringsAsFactors=F))
    dat2 <- do.call(rbind, dat2)
    dat2$design <- as.factor(dat2$design)
    plot2 <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data=dat2, ggplot2::aes(x=design, y=rec, fill=design), alpha=0.5, lwd=0.2, outlier.shape=NA) +
      ggplot2::geom_jitter(data=dat2, ggplot2::aes(x=design, y=rec), shape=16, alpha=0.3, width=0.25, height=0) +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      ggplot2::ylab("# unique rec.") +
      ggplot2::labs(tag="B")
    
    # plot3: proportions of individual recombinant haplotypes.
    if(is.null(fpair)) fpair <- cbind(1, 2:n)
    fpair <- paste(fpair[,1], fpair[,2], sep="_")

    dat3 <- lapply(1:n.design, FUN=function(x) data.frame(design=x, sim=1:n.sim, t(input[[x]][[2]][fpair, , drop=F]), stringsAsFactors=F))
    dat3 <- do.call(rbind, dat3)
    dat3 <- reshape2::melt(dat3, id.vars=c("sim", "design"))
    dat3$design <- as.factor(dat3$design)
    dat3$design <- factor(dat3$design, labels=paste("design", 1:n.design, sep=" "))
    dat3$design <- factor(dat3$design, levels=rev(levels(dat3$design)))
    levels(dat3$variable) <- gsub("X", "", levels(dat3$variable))

    hues <- seq(15, 375, length=n.design+1)
    hues <- hcl(h=hues, l=65, c=100)[1:n.design]
    
    plot3 <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data=dat3, ggplot2::aes(x=variable, y=value, fill=design), alpha=0.5, lwd=0.2,  outlier.alpha=0.3, outlier.size=1) +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      ggplot2::xlab("recombinant haplotypes") +
      ggplot2::ylab("proportion") +
      ggplot2::scale_x_discrete(limits=rev(levels(dat3$variable))) +
      ggplot2::scale_fill_manual(values=rev(hues)) +
      ggplot2::coord_flip() +
      ggplot2::labs(tag="C")
    
    # legend: get the common legend for plot1-3.
    temp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot1))
    legend <- temp$grobs[[which(sapply(temp$grobs, function(x) x$name) == "guide-box")]]
    
    # remove the legend in plot1-3.
    plot1 <- plot1 + ggplot2::theme(legend.position="none")
    plot2 <- plot2 + ggplot2::theme(legend.position="none")
    plot3 <- plot3 + ggplot2::theme(legend.position="none")
    
    # combine plot1-3 and legend.
    gridExtra::grid.arrange(plot1, plot2, legend, plot3, layout_matrix=matrix(c(1,1,2,2,3,4,4,4,4,4), nrow=2, byrow=T))

  } else if(display=="whole"){
    
    # plot4: proportion of founder genome in the final MAGIC population.
    dat4 <- vector()
    for(i in 1:n.design){
      for(j in 1:n){
        dat4 <-  rbind(dat4, cbind(colSums(input[[i]][[3]][[j]])/nrow(input[[i]][[3]][[j]]), i, j))
      }
    }
    dat4 <- data.frame(dat4, stringsAsFactors=F)
    colnames(dat4) <- c("proportion", "design", "founder")
    dat4$design <- as.factor(dat4$design)
    dat4$founder <- as.factor(dat4$founder)
    
    plot4 <- ggplot2::ggplot() +
      ggplot2::annotate("segment", x=-Inf, xend=Inf, y=1/n, yend=1/n, color="#555555", linetype=3) +
      ggplot2::geom_boxplot(data=dat4, ggplot2::aes(x=founder, y=proportion, fill=design), alpha=0.5, lwd=0.2, outlier.alpha=0.3, outlier.size=1) +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      ggplot2::annotate("segment", x=seq(1.5, n, 1), xend=seq(1.5, n, 1), y=-Inf, yend=Inf, color="#EEEEEE") +
      ggplot2::ylab("prop.") +
      ggplot2::xlab("founder") +
      ggplot2::labs(tag="A")
    
    # plot5: proportions of 1 to n unique founder alleles in the final population (for each chromosome).
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
    
    dat5 <- vector()
    for(i in 1:n.design){
      for(j in 1:(n.chr+1)){
        dat5 <- rbind(dat5, cbind(sapply(1:n, FUN=function(x) colSums(temp[[i]][[j]]==x)/nrow(temp[[i]][[j]])), i, j))
      }
    }
    
    dat5 <- reshape2::melt(data.frame(dat5, stringsAsFactors=F), id.vars=c("i", "j"))
    colnames(dat5) <- c("design", "chr", "founder", "prop.")
    levels(dat5$founder) <- gsub("V", "", levels(dat5$founder))
    dat5$design <- as.factor(dat5$design)
    dat5$chr <- as.factor(dat5$chr)
    if(is.null(chr.names)){
      dat5$chr <- factor(dat5$chr, labels=c(paste("chr", 1:n.chr, sep=" "), "all"))
    } else {
      dat5$chr <- factor(dat5$chr, labels=c(paste("chr", chr.names, sep=" "), "all"))
    }
    
    plot5 <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data=dat5, ggplot2::aes(x=founder, y=prop., fill=design), alpha=0.5, lwd=0.2, outlier.alpha=0.3, outlier.size=1) +
      ggplot2::facet_wrap(ggplot2::vars(chr), ncol=3, scales="free_y") +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      ggplot2::annotate("segment", x=seq(1.5, n, 1), xend=seq(1.5, n, 1), y=-Inf, yend=Inf, color="#EEEEEE") +
      ggplot2::xlab("number of unique founders") +
      ggplot2::theme(legend.position="none") +
      ggplot2::labs(tag="B")

    
    # plot6: densities of segment lengths in each design.
    dat6 <- vector()
    for(i in 1:n.design){
      for(j in 1:n.chr){
        seg.mean <- rowSums(input[[i]][[5]][[j]])/n.sim
        seg.sd <- sqrt(rowSums((input[[i]][[5]][[j]] - seg.mean)^2)/n.sim)
        dat6 <- rbind(dat6,
                      data.frame(seg.len=1:nrow(input[[i]][[5]][[j]]),
                                 seg.mean=seg.mean,
                                 seg.sd=seg.sd,
                                 design=i,
                                 chr=j))
      }
    }
    
    dat6$design <- as.factor(dat6$design)
    dat6$chr <- as.factor(dat6$chr) 
    if(is.null(chr.names)){
      levels(dat6$chr) <- paste("chr", levels(dat6$chr), sep=" ")
    } else {
      levels(dat6$chr) <- paste("chr", chr.names, sep=" ")
    }
    
    plot6 <- ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=NA, color="#DDDDDD") +
      #ggplot2::geom_point(data=dat6, ggplot2::aes(x=seg.len, y=seg.mean, color=design), alpha=0.5, shape=16) +
      ggplot2::geom_smooth(data=dat6, ggplot2::aes(x=seg.len, y=seg.mean, color=design), se=F, alpha=0.5) +
      ggplot2::facet_wrap(ggplot2::vars(chr), ncol=3, scales="free") +
      ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(expand=c(0.02,0)) +
      ggplot2::scale_y_continuous(expand=c(0.02,0)) +
      ggplot2::xlab("non-recombinant segment length (cM)") +
      ggplot2::ylab("mean count") +
      ggplot2::theme(legend.position="none") +
      ggplot2::labs(tag="C")
    
    # combine plot4-6.
    suppressMessages(gridExtra::grid.arrange(plot4, plot5, plot6, heights=c(1, ceiling((n.chr+1)/3), ceiling(n.chr/3))))
  }
  
  message("Note: the plot can be exported using the ggsave function in ggplot2, for example, ggplot2::ggsave(filename=\"plot.png\", width=7, height=7, units=\"in\", dpi=600)")

}