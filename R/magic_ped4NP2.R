#' Plot pedigree.
#'
#' This function takes a pedigree and displays it in an interactive plot. This function
#' works for any pedigree including user-provided pedigree and number of founders not
#' of a power of 2.
#'
#' @param ped a pedigree.
#' @param w2h.ratio a numerical value of width-to-height-ratio in the pedigree plot.
#' @return a HTML file of an interactive pedigree plot.
#'
#' @noRd

magic.ped4NP2 <- function(ped, w2h.ratio=2){

  # assign column names to ped.
  colnames(ped) <- c("id","p1","p2","gen")
  ped <- data.frame(ped, stringsAsFactors=FALSE)
  ped$gen <- as.numeric(ped$gen)
  
  # identify the number of founder (n) and total generations (n.gen).
  n <- sum(ped$gen==0)
  n.gen <- max(ped$gen)
  
  # trace the predecessors of each RIL in the final generation.
  lineage <- vector()
  for(i in which(ped$gen==n.gen)){
    temp1 <- temp2 <- i
    for(j in (n.gen-1):0){
      temp <- cbind(ped$p1[temp1], ped$p2[temp1])
      temp <- c(temp[temp[,1]==temp[,2],1],
                c(temp[!(temp[,1]==temp[,2]),]))
      temp1 <- unname(sapply(temp, FUN=function(x) which(ped$id==x)))
      temp2 <- c(temp2, temp1)
    }
    lineage <- rbind(lineage, sort(temp2))
  }
  
  # set the distance between individuals in each generation to 0.5.
  ped.dist <- lapply(unique(ped$gen), FUN=function(x) rep(0.5, sum(ped$gen==x)))
  
  # calculate the width of each generation.
  ped.width <- sapply(1:length(ped.dist), FUN=function(x) sum(ped.dist[[x]]) + 0.5*length(ped.dist[[x]]) + ped.dist[[x]][length(ped.dist[[x]])])
  
  # scale the distances to match with the maximum width.
  ped.dist <- lapply(1:length(ped.dist), FUN=function(x) ped.dist[[x]]*(max(ped.width)-0.5*length(ped.dist[[x]]))/(sum(ped.dist[[x]]) + ped.dist[[x]][length(ped.dist[[x]])]))
  
  # move the last distance in each generation to the first position so the spacing is centered.
  ped.dist <- lapply(1:length(ped.dist), FUN=function(x) c(ped.dist[[x]][length(ped.dist[[x]])], ped.dist[[x]][-length(ped.dist[[x]])]))
  
  # calculate the position of each individual (currently, the plot is set to width=2*height, adjust y if needed).
  pos <- vector()
  for(i in 1:length(ped.dist)){
    for(j in 1:length(ped.dist[[i]])){
      pos <- rbind(pos, c(sum(ped.dist[[i]][1:j]) + 0.25*(2*j-1), max(ped.width)/max(ped$gen)*(length(ped.dist)-i+1)/w2h.ratio))
    }
  }
  pos <- data.frame(pos, stringsAsFactors=FALSE)
  colnames(pos) <- c("x", "y")
  
  # get the final RIL/funnel names.
  temp <- ped$id[ped$gen==n.gen]
  
  # here, we expand pos to cover each funnel separately.
  pos <- data.frame(gen=ped$gen, pos, stringsAsFactors=FALSE)
  temp.pos <- vector()
  for(i in 1:nrow(lineage)){
    temp.pos <- rbind(temp.pos, cbind(pos[lineage[i,], , drop=FALSE], funnel=temp[i]))
  }
  
  # calculate rship, where x1/y1 are the progeny x/y position, x2/y2 are the parent x/y position.
  # notice we rbind two data.frame, the top is to draw the line between progeny and parent 1,
  # and the bottom is to draw the line between progeny and parent 2.
  idx <- ped[!(ped$gen==0), ]
  idx <- cbind(sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$id[x])),
               sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$p1[x])),
               sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$p2[x])))
  rship <- vector()
  for(i in 1:nrow(lineage)){
    temp.lineage <- lineage[i,]
    temp.lineage <- temp.lineage[temp.lineage > n] - n
    temp.idx <- idx[temp.lineage, ]
    rship <- rbind(rship, rbind(data.frame(x1=pos$x[temp.idx[,1]],
                                           y1=pos$y[temp.idx[,1]],
                                           x2=pos$x[temp.idx[,2]],
                                           y2=pos$y[temp.idx[,2]],
                                           funnel=temp[i],
                                           stringsAsFactors=FALSE),
                                data.frame(x1=pos$x[temp.idx[,1]],
                                           y1=pos$y[temp.idx[,1]],
                                           x2=pos$x[temp.idx[,3]],
                                           y2=pos$y[temp.idx[,3]],
                                           funnel=temp[i],
                                           stringsAsFactors=FALSE)))
  }
  
  pos <- temp.pos
  pos$funnel <- as.factor(pos$funnel)
  rship$funnel <- as.factor(rship$funnel)
  
  # create a new data.frame just to store the founder for labelling the plot.
  flabel <- unique(pos[pos$gen==0, c("x","y")])
  flabel <- flabel[order(flabel$x),]
  if(max(flabel$y) > 10){
    flabel$y <- flabel$y*1.05
  } else {
    flabel$y <- flabel$y + 0.5
  }
  flabel$founder <- 1:n
  
  # plot.
  pos.ly <- plotly::highlight_key(pos, ~funnel)
  rship.ly <- plotly::highlight_key(rship, ~funnel)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data=pos.ly, ggplot2::aes(xmin=x-0.25, xmax=x+0.25, ymin=y-0.25, ymax=y+0.25, group=funnel), fill="#DDDDDD", color="#505050") +
    ggplot2::geom_segment(data=rship.ly, ggplot2::aes(x=x1, xend=x2, y=y1+0.25, yend=y2-0.25, group=funnel), color="#505050") +
    ggplot2::annotate("text", x=flabel$x, y=flabel$y, label=flabel$founder) +
    ggplot2::theme(panel.background=ggplot2::element_blank(), panel.grid=ggplot2::element_blank()) +
    ggplot2::theme(axis.title=ggplot2::element_blank(), axis.text=ggplot2::element_blank(), axis.ticks=ggplot2::element_blank()) +
    ggplot2::coord_fixed(ratio=1)
  p <- plotly::ggplotly(p, tooltip="funnel")
  p <- plotly::config(p, displaylogo=FALSE, toImageButtonOptions=list(filename="pedigree", width=4200, height=floor(4200/w2h.ratio))) #width is equivalent to 7 in at 600dpi
  p <- plotly::highlight(p, on="plotly_click", off="plotly_doubleclick", color="#FF5050")

  # save the output in html format.
  #htmlwidgets::saveWidget(p, paste(filename, ".html", sep=""), selfcontained=TRUE)
  #if(grepl("/", filename, fixed=TRUE)){
  #  message(paste("pedigree plot has been saved to ", filename, ".html", sep=""))
  #}
  #else {
  #  message(paste("pedigree plot has been saved to ", getwd(), "/", filename, ".html", sep=""))
  #}
  
  return(p)
}