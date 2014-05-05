bin.signif <- function(x){
  # x is a matrix, a contingentcy table
  gr1 <- rowSums(x)
  gr2 <- colSums(x)
  N <- sum(x)
  # compute H0:
  H0 <- (gr1%*%t(gr2))/N
  # p:
  return(pbinom(x, N, ((gr1%*%t(gr2))/N^2)))
}
bin.signif <- compiler::cmpfun(bin.signif)


chooseSignificantChanges <- function(list.of.roles, name.of.property, signif.threshold=0.001){
  time <- as.POSIXlt(sapply(lapply(lapply(list.of.roles, "[[", "data"), "[[", "timestamp"),function(v){v[length(v)]}), origin="1970-01-01")
  pairs.list <- NULL
  chisq.p <- numeric(length(list.of.roles)-1)
  diff.of.cls <- NULL
  cls.prop <- NULL
  for(i in 2: length(list.of.roles)){
    cl1 <- list.of.roles[[i-1]]
    cl2 <- list.of.roles[[i]]

    # order them by name.of.property
    cl1.cls <- cl1$clusters[order(cl1$clusters[[name.of.property]]),]
    cl2.cls <- cl2$clusters[order(cl2$clusters[[name.of.property]]),]

    # get significant pairs
    mbrs <- merge(cl1$users[c("user", "membership")],
                  cl2$users[c("user", "membership")],
                  by="user",
                  all=TRUE)
    chisq.p[i-1] <- chisq.test(as.matrix(table(mbrs[2:3], useNA="always")))$p.value
    pairs <- bin.signif(as.matrix(table(mbrs[2:3], useNA="always")))
    pairs.list <- c(pairs.list, list(pairs))
    signif.pairs <- data.frame(from=dimnames(pairs)[[1]][which(pairs>(1-signif.threshold), arr.ind=TRUE)[,1]],
                               to=dimnames(pairs)[[2]][which(pairs>(1-signif.threshold), arr.ind=TRUE)[,2]])
    if(nrow(signif.pairs)){
      dimnames(signif.pairs)[[1]] <- 1:nrow(signif.pairs)
    }
    diff.of.cls <- c(diff.of.cls, list(signif.pairs))
    clsp <- list(cl1$clusters[[name.of.property]],
                 cl2$clusters[[name.of.property]])
    names(clsp) <- c(paste0(name.of.property, "_1"), paste0(name.of.property, "_2"))
    names(clsp[[1]]) <- cl1$cluster$cluster
    names(clsp[[2]]) <- cl2$cluster$cluster
    cls.prop <- c(cls.prop, list(clsp))
  #cat(paste0("\n", i))
  }
  return(list(times=time,
              pairs=pairs.list,
              chisq=chisq.p,
              signif.pairs=diff.of.cls,
              cls.prop=cls.prop))
}

chooseSignificantChanges <- compiler::cmpfun(chooseSignificantChanges)


plotSignificantChanges <- function(chs, ylab, pch=16){
  if(missing(ylab)){
    ylab=names(chs$cls.prop[1][1])
  }
  Xs <- chs$times
  max.prop <- max(sapply(sapply(chs$cls.prop, unlist), max))
  min.prop <- min(sapply(sapply(chs$cls.prop, unlist), min))
  new.group <- max.prop*(-10)
  lost.group <- max.prop*(10)
  # Drawing forst set of groups, creating the plot
  plot(rep(Xs[1], length(chs$cls.prop[[1]][[1]])),
       chs$cls.prop[[1]][[1]],
       xlim=c(as.numeric(min(Xs)), as.numeric(max(Xs))),
       ylim=c(min.prop, max.prop),
       xlab="Date",
       ylab=ylab,
       main="Change in group properties",
       pch=pch)
  # plot the position of clusters
  for(i in 2:length(chs$times)){
    points(rep(Xs[i], length(chs$cls.prop[[i-1]][[2]])),
           chs$cls.prop[[i-1]][[2]],
           pch=pch)
  }
  # draw lines between clusters where the connetction is significant
  for(i in 1:length(chs$signif.pairs)){
    if(nrow(chs$signif.pairs[[i]])){
      y1 <- chs$cls.prop[[i]][[1]][chs$signif.pairs[[i]][,"from"]]
      y1[is.na(y1)] <- new.group
      y2 <- chs$cls.prop[[i]][[2]][chs$signif.pairs[[i]][,"to"]]
      y2[is.na(y2)] <- lost.group
      for(l in 1:length(y1)){
        lines(x=Xs[i:(i+1)],
              y=c(y1[l], y2[l]),
              col=ifelse(any(c(y1[l],y2[l])==new.group), "red",
                    ifelse(any(c(y1[l],y2[l])==lost.group), "grey", "black")))
      }
    }
  }
}