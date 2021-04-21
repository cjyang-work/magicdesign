#' Plot pedigree.
#'
#' This function takes a pedigree and displays it in an interactive plot. This function
#' only works for number of founders that are power of 2.
#'
#' @param ped a pedigree.
#' @param basic a logical indicator of whether it is a basic design or not.
#' @param show.partial a logical indicator of whether to plot a partial pedigree or not.
#' @param w2h.ratio a numerical value of width-to-height-ratio in the pedigree plot.
#' @return a HTML file of an interactive pedigree plot.
#'
#' @noRd

magic.ped4perfect <- function(ped, basic=FALSE, show.partial=FALSE, w2h.ratio=2){

  # assign column names to ped.
  colnames(ped) <- c("id","p1","p2","gen")
  ped <- data.frame(ped, stringsAsFactors=FALSE)
  ped$gen <- as.numeric(ped$gen)

  # identify the number of founder (n), crossing generations (nx) and total generations (n.gen).
  n <- sum(ped$gen==0)
  if(!(n %in% c(4,8,16,32,64,128))) stop("accepted n: 4, 8, 16, 32, 64, 128")
  nx <- log(n,2)
  n.gen <- max(ped$gen)
  
  # identify crossing vs selfing generations.
  s.gen <- unique(ped$gen[grep("s", ped$id)])
  x.gen <- if(length(s.gen)==0) 1:n.gen else c(1:n.gen)[-s.gen]
    
  # create a new column of id with replicate number removed.
  ped$id1 <- gsub("_.*", "", ped$id)
  ped$id2 <- ped$id
  k <- 1
  for(i in 1:n.gen){
    if(i %in% s.gen){
      ped$id2[ped$gen==i] <- paste(gsub(paste(LETTERS[k-1],".*",sep=""), "", ped$id[ped$gen==i]),
                                   gsub(".*(s)", "\\1", ped$id[ped$gen==i]),
                                   sep="")
    } else {
      ped$id2[ped$gen==i] <- gsub(paste(LETTERS[k],".*",sep=""), "", ped$id[ped$gen==i])
      k <- k + 1
    }
  }
  
  # create new column of id in numeric form, each individual has a unique id.
  # also create a new column of full-sibs, any full-sibs share the same id.
  ped$id3 <- 1:nrow(ped)
  ped$sib <- 0
  k <- 1
  for(i in 1:nrow(ped)){
    temp <- ped$id2==ped$id2[i] & ped$p1==ped$p1[i] & ped$p2==ped$p2[i]
    if(sum(temp) > 1 & all(ped$sib[temp]==0)){
      ped$sib[temp] <- k
      k <- k + 1
    }
  }
  
  # trace the predecessors of each RIL in the final generation.
  lineage <- vector()
  for(i in which(ped$gen==n.gen)){
    temp1 <- temp2 <- i
    for(j in (n.gen-1):0){
      temp1 <- which(ped$id %in% ped$p1[temp1] | ped$id %in% ped$p2[temp1])
      temp2 <- c(temp2, temp1)
    }
    lineage <- rbind(lineage, sort(temp2))
  }
  
  # identify the unique RIL (no replication in the final crossing generation).
  temp <- ped[ped$gen==max(x.gen),]
  ril.unique <- vector()
  if(all(temp$sib>0)){
    for(i in 1:nrow(temp)){
      if(sum(temp$sib[1:i]==temp$sib[i]) == 1) ril.unique <- c(ril.unique, temp$id3[i])
    }
  } else {
    ril.unique <- temp$id3
  }
  
  # keep only unique RIL, remove any replicates from the final crossing generation to de-clutter the pedigree plot.
  lineage <- lineage[sapply(1:nrow(lineage), FUN=function(x) any(ril.unique%in%lineage[x,])), , drop=FALSE]
  ped <- ped[sort(unique(c(lineage))), ]
  
  # if basic=FALSE, n=8 and show.partial=TRUE, we will find one partial funnel set to plot
  if(!basic & n == 8 & show.partial){

    # get the founder combinations in the final funnel.
    fmat <- matrix(as.numeric(c(stringr::str_split_fixed(unique(ped$id1[ped$gen==n.gen]),";",n))), ncol=n)
    
    # determine the generation at which two founders meet.
    # 1: founders meet at 2-way cross.
    # 2: founders meet at 4-way cross.
    # 3: founders meet at 8-way cross, and so on.
    m <- nrow(fmat)/(n-1)
    comb <- utils::combn(n,2)
    grp <- sapply(1:nx, FUN=function(x) sort(rep(1:2^(nx-x),2^x)))
    mat <- matrix(nx+1, nrow=nrow(fmat), ncol=ncol(comb))
    for(i in 1:nrow(fmat)){
      for(j in 1:ncol(comb)){
        temp <- which(fmat[i,]%in%comb[,j])
        mat[i,j] <- mat[i,j] - sum(sapply(1:ncol(grp), FUN=function(x) length(unique(grp[temp,x])))==1)
      }
    }
    
    # loop to identify funnel sets in balanced partial design.
    # currently, we only plot ONE random funnel set (m0), if we need to plot all sets, remove m0 and add "for(i in 1:m)" above fset.
    m0 <- sample(1:m, 1)
    idx <- lapply(1:(n-1), FUN=function(x) which(mat[,x]==1))
    
    fset <- idx[[1]][m0]
    temp <- idx[-1]
    j <- 1
    k <- rep(1, n-2)
    k0 <- 1
    while(length(fset) < n-1){
      fset <- c(fset, temp[[j]][k[j]])
      if(any(sapply(1:nx, FUN=function(x) any(colSums(mat[fset,,drop=FALSE]==x) > 2^(x-1))))){
        fset <- fset[-length(fset)]
        if(j == 1){
          k0 <- k0 + 1
          k[j] <- k0
        } else {
          
          k[j] <- k[j] + 1
          while(k[j] > m & !(j==1)){
            fset <- fset[-length(fset)]
            k[j] <- 1
            j <- j - 1
            k[j] <- k[j] + 1
            if(j == 1){
              k0 <- k0 + 1
              k[j] <- k0
            }
          }
        }
        
      } else {
        j <- j + 1
      }
    }
    
    
    # keep only individuals that are found in a funnel set.
    fset <- do.call(paste, c(data.frame(fmat[fset,], stringsAsFactors=FALSE), sep=";"))
    
    # identify and keep the RILs that make a funnel set.
    temp <- sapply(1:length(fset), FUN=function(x) max(ped$id3[ped$id1==fset[x]]))
    lineage <- lineage[sapply(1:nrow(lineage), FUN=function(x) any(temp%in%lineage[x,])), , drop=FALSE]
    ped <- ped[ped$id3 %in% sort(unique(c(lineage))), ]
    
  }
  
  # reorder ped in each generation to keep siblings together and tidy up plot.
  temp <- stringr::str_split_fixed(ped$id1, ";", 2^length(x.gen))
  temp <- sapply(1:ncol(temp), FUN=function(x) stringr::str_pad(temp[,x], nchar(n), pad="0"))
  temp <- do.call(paste,c(data.frame(temp, stringsAsFactors=FALSE), sep=""))
  ped <- ped[order(ped$gen, temp), ]
  
  # set the distance between individuals in each generation to 0.5 (or 0 if they are sibs).
  ped.dist <- vector()
  for(i in 1:nrow(ped)){
    if(sum(ped$id2[i:nrow(ped)]==ped$id2[i]) > 1){
      ped.dist <- c(ped.dist, 0)
    } else {
      ped.dist <- c(ped.dist, 0.5)
    }
  }
  
  # split the distances based on generations.
  ped.dist <- lapply(unique(ped$gen), FUN=function(x) ped.dist[ped$gen==x])
  
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

  # here, we expand pos to cover each funnel separately.
  pos <- data.frame(id3=ped$id3, gen=ped$gen, pos, stringsAsFactors=FALSE)
  
  # add an extra index to distinguish among replicated funnels (can occur in some cases, e.g. basic=TRUE).
  temp <- sapply(1:nrow(lineage), FUN=function(x) ped$id1[ped$id3 == lineage[x,ncol(lineage)]])
  temp2 <- unique(temp)
  for(i in 1:length(temp2)){
    if(sum(temp==temp2[i]) > 1){
      temp[temp==temp2[i]] <- paste(temp[temp==temp2[i]], " (", 1:sum(temp==temp2[i]), ")", sep="")
    }
  }
  
  temp.pos <- vector()
  for(i in 1:nrow(lineage)){
    temp.pos <- rbind(temp.pos, cbind(pos[pos$id3 %in% lineage[i,], , drop=FALSE], funnel=temp[i]))
  }
  
  # calculate rship, where x1/y1 are the progeny x/y position, x2/y2 are the parent x/y position.
  # notice we rbind two data.frame, the top is to draw the line between progeny and parent 1,
  # and the bottom is to draw the line between progeny and parent 2.
  idx <- ped[!(ped$gen==0), ]
  idx <- cbind(ped$id3[sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$id[x]))],
               ped$id3[sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$p1[x]))],
               ped$id3[sapply(1:nrow(idx), FUN=function(x) which(ped$id==idx$p2[x]))])
  rship <- vector()
  for(i in 1:nrow(lineage)){
    temp.idx <- idx[idx[,1] %in% lineage[i,], ]
    temp1 <- sapply(1:nrow(temp.idx), FUN=function(x) which(pos$id3==temp.idx[x,1]))
    temp2 <- sapply(1:nrow(temp.idx), FUN=function(x) which(pos$id3==temp.idx[x,2]))
    temp3 <- sapply(1:nrow(temp.idx), FUN=function(x) which(pos$id3==temp.idx[x,3]))
    
    rship <- rbind(rship, rbind(data.frame(x1=pos$x[temp1],
                                           y1=pos$y[temp1],
                                           x2=pos$x[temp2],
                                           y2=pos$y[temp2],
                                           funnel=temp[i],
                                           stringsAsFactors=FALSE),
                                data.frame(x1=pos$x[temp1],
                                           y1=pos$y[temp1],
                                           x2=pos$x[temp3],
                                           y2=pos$y[temp3],
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