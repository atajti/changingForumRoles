slice <- function(data, minimum.posts, step.size, step.nr){
  first <- (step.nr-1) * step.size + 1
  last <- (step.nr-1)*step.size + minimum.posts
  dat <- data[1:last,]
  dat <- dat[dat$nid %in% dat$nid[first:last],]
}

slice <- compiler::cmpfun(slice)

getClusterNumber <- function(measures){
  # get the optimal number of clusters by choosing the most supperted one.
  # measures is a list of vectors, preferably with same length
  mins <- unlist(sapply(measures, getKnee))
  return(as.numeric(names(which.max(table(mins)))))
}

getClusterNumber <- compiler::cmpfun(getClusterNumber)


analyzeRoles <- function(data){
  users <- computeUserFeatures(data)
  # get 3 principal components
  pri <- princomp(as.matrix(users[[1]]))
  # create clustering space
  scores <- t(t(pri$scores[, 1:3])*pri$sdev[1:3])
  # hierarchical clustering
  hc <- hclust(dist(scores), method="complete")
  # search for ideal cluster size:
  cluster.scores <- NULL
  for(i in 1:29){
    cluster.scores <- c(cluster.scores, list(fpc::cluster.stats(dist(scores),
                                                                cutree(hc, k=(i+1)))))
  }
  silhouette.res <- scale(sapply(cluster.scores, "[[", "avg.silwidth"))
  pearsongamma.res <- scale(sapply(cluster.scores, "[[", "pearsongamma"))
  dunn.res <- scale(sapply(cluster.scores, "[[", "dunn"))
  dunn2.res <- scale(sapply(cluster.scores, "[[", "dunn2"))
  wb.ratio.res <- scale(sapply(cluster.scores, "[[", "wb.ratio"))
  ch.res <- scale(sapply(cluster.scores, "[[", "ch"))
  sindex.res <- scale(sapply(cluster.scores, "[[", "sindex"))
  clnum <- getClusterNumber(list(silhouette.res, pearsongamma.res, dunn.res, 
                                 dunn2.res, wb.ratio.res, ch.res, sindex.res)) + 1
  # create users' feature and membership
  users[[1]]$user <- users[[2]]
  cl.props <- merge(users[[1]],
                    data.frame(user=users[[1]]$user,
                               membership=cutree(hc, k=clnum)),
                    by="user", all=TRUE)
  # compute summary stats for clusters:
  suppressWarnings(
    cldat <- Reduce(function(x,y){
                      merge(x,y, by="Group.1", all=TRUE)
                    },
                    list(#aggregate(cl.props$parent.indeg.mean, by=list(cl.props$membership), "sd"),
                         aggregate(cl.props$parent.indeg.mean, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$parent.indeg.std, by=list(cl.props$membership), "sd"),
                         #aggregate(cl.props$parent.indeg.std, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$parent.outdeg.mean, by=list(cl.props$membership), "sd"),
                         aggregate(cl.props$parent.outdeg.mean, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$parent.outdeg.std, by=list(cl.props$membership), "sd"),
                         #aggregate(cl.props$parent.outdeg.std, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$serial.sd, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$serial.sd, by=list(cl.props$membership), "sd"),
                         aggregate(cl.props$serial.mean, by=list(cl.props$membership), "mean"),
                         #aggregate(cl.props$serial.mean, by=list(cl.props$membership), "sd"),
                         aggregate(cl.props$size, by=list(cl.props$membership), "mean")#,
                         #aggregate(cl.props$size, by=list(cl.props$membership), "sd")))
                         )))
  names(cldat) <- c("cluster",
                    #"parent.indeg.mean.sd",
                    "parent.indeg.mean.mean",
                    #"parent.indeg.std.sd", "parent.indeg.std.mean",
                    #"parent.outdeg.mean.sd",
                    "parent.outdeg.mean.mean",
                    #"parent.outdeg.std.sd", "parent.outdeg.std.mean",
                    #"serial.sd.mean", "serial.sd.sd",
                    "serial.mean.mean",# "serial.mean.sd",
                    "size.mean")#, "size.sd")
  cldat <- merge(cldat, as.data.frame(table(cl.props$membership)),
                 by.x="cluster", by.y="Var1")
  # return datasets:
  return(list(data=data,
              users=cl.props,
              clusters=cldat))
}

analyzeRoles <- compiler::cmpfun(analyzeRoles)