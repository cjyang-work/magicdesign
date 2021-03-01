#' Produce all balanced partial design for 8 founders.
#'
#' This function takes nothing and produces all possible balanced partial design
#' for n=8 and m=1, i.e. 8 founders and 1 funnel set. It can be slow so the output
#' is provided in R/sysdata.rda.
#'
#' @return a matrix of all possible individual combinations that would make a
#'         balanced partial design.
#'
#' @noRd

magic.db <- function(){

  # get the full design for n=8.
  xinfo <- magic.full(n=8)

  # get the founder combinations for 2-way and 8-way.
  xinfo.1 <- t(xinfo[[1]][[1]])
  xinfo.3 <- t(xinfo[[1]][[3]])

  # get the count of pairing between any two founders.
  db1 <- vector()
  db2 <- vector()
  
  for(i in 1:28){
    db1 <- rbind(db1, (colSums(xinfo.3[c(1,2),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(2,1),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(3,4),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(4,3),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(5,6),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(6,5),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(7,8),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(8,7),]==xinfo.1[,i])==2))
    db2 <- rbind(db2, (colSums(xinfo.3[c(1,3),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(3,1),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(1,4),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(4,1),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(2,3),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(3,2),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(2,4),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(4,2),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(5,7),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(7,5),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(5,8),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(8,5),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(6,7),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(7,6),]==xinfo.1[,i])==2 |
                         colSums(xinfo.3[c(6,8),]==xinfo.1[,i])==2 | colSums(xinfo.3[c(8,6),]==xinfo.1[,i])==2))
  }
  
  db1[db1==TRUE] <- 1
  db1[db1==FALSE] <- 0
  
  db2[db2==TRUE] <- 1
  db2[db2==FALSE] <- 0
  
  db1 <- list(db1)
  db2 <- list(db2)
  idx <- list(1:ncol(db1[[1]]))

  # loop to find all possible balanced partial designs.
  k <- 1
  out <- vector()
  
  while(T){
    
    if(k < 7 & length(idx[[k]]) > 0) {
      db1[[k+1]] <- db1[[k]] + db1[[1]][,idx[[k]][1]]
      db2[[k+1]] <- db2[[k]] + db2[[1]][,idx[[k]][1]]
      idx[[k+1]] <- which(colSums(db1[[k+1]] > 1)==0 & colSums(db2[[k+1]] > 2)==0)
      idx[[k+1]] <- idx[[k+1]][idx[[k+1]] > idx[[k]][1]]
      k <- k + 1
      
    } else if(k == 7 & length(idx[[k]]) > 0) {
      out <- rbind(out,
                   t(sapply(1:length(idx[[7]]), FUN=function(x) c(sapply(idx[1:6], "[[", 1), idx[[7]][x]))))
      k <- k - 1
      db1 <- db1[1:k]
      db2 <- db2[1:k]
      idx  <- idx[1:k]
      idx[[k]] <- idx[[k]][-1]
      
    } else if(length(idx[[k]]) == 0) {
      k <- k - 1
      db1 <- db1[1:k]
      db2 <- db2[1:k]
      idx  <- idx[1:k]
      idx[[k]] <- idx[[k]][-1]
      
    }
    
    if(length(idx[[1]]) < 7) {
      break
    }
    
  }
  
  # set the output to a class called "db".
  class(out) <- "db"
  
  return(out)
}