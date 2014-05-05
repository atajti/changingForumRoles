# sclicer, PCAer:
pd <- read.csv("cm_ranks.csv")
pd <- pd[order(pd$timestamp),]
# slicer:
slice <- function(data, minimum.posts, step.size, step.nr){
  first <- (step.nr-1) * step.size + 1
  last <- (step.nr-1)*step.size + minimum.posts
  dat <- data[1:last,]
  dat <- dat[dat$nid %in% dat$nid[first:last],]
}
slice <- compiler::cmpfun(slice)
# compute features for each user:
library(igraph)
source("functions/createReplyGraph.R")
source("functions/rankSearcher.R")
source("functions/computeUserFeatures2.R")
source("functions/getKnee.R")

# check data from the beginning in a low and a high activity period
# in the beginnig and at the end:
beg.slow <- slice(pd, 1000, 500, 5)
beg.act <- slice(pd, 1000, 500, 20)
end.slow <- slice(pd, 1000, 500, 720)
end.act <- slice(pd, 1000, 500, 673)

nrow(beg.slow)
as.POSIXlt(c(min(beg.slow$timestamp), max(beg.slow$timestamp)), origin="1970-01-01")
sum(is.na(beg.slow$pid))
length(unique(beg.slow$user))

nrow(beg.act)
as.POSIXlt(c(min(beg.act$timestamp), max(beg.act$timestamp)), origin="1970-01-01")

# PCAs:
library(rrcov)
## beg.slow
user.names <- computeUserFeatures(beg.slow)
b.s.user <- as.matrix(scale(user.names[["features"]]))
user.names <- user.names[["users"]]
nrow(b.s.user)
pri <- princomp(b.s.user)
par(mfrow=c(1,3))
plot(pri, main="Scree plot of regular PCA\nfor early stagnating times")
# prm <- princomp(b.s.user, covmat=MASS::cov.mcd(b.s.user))
# plot(prm, main="Scree plot of PCA with MCD\nfor early stagnating times")
prc <- prcomp(b.s.user, scale=TRUE)
plot(prc, main="Scree plot of rotated PCA\nfor early stagnating times")
pcr <- PcaHubert(b.s.user)
screeplot(pcr, main="Scree plot of ROBPCA\nfor early stagnating times")
sort(abs(pcr@loadings[,1]))
names(which(abs(pcr@loadings[,1])>.01))
names(which(abs(pcr@loadings[,2])>.01))
# beg.act
user.names <- computeUserFeatures(beg.act)
b.a.user <- as.matrix(scale(user.names[["features"]]))
user.names <- user.names[["users"]]
nrow(b.a.user)
par(mfrow=c(1,3))
pri <- princomp(b.a.user)
plot(pri, main="Scree plot of regular PCA\nfor early times around demonstration")
# prm <- princomp(b.a.user, covmat=MASS::cov.mcd(b.a.user))
# plot(prm, main="Scree plot of PCA with MCD\nfor early times around demonstration")
prc <- prcomp(b.a.user, scale=TRUE)
plot(prc, main="Scree plot of rotated PCA\nfor early times around demonstration")
pcr <- PcaHubert(b.a.user)
sort(abs(pcr@loadings[,1]))
screeplot(pcr, main="Scree plot of ROBPCA\nfor early times around demonstration")
names(which(abs(pcr@loadings[,1])>.01))
names(which(abs(pcr@loadings[,2])>.01))
# end.slow
  nrow(end.slow)
  sum(is.na(end.slow$pid))
  as.POSIXlt(c(min(end.slow$timestamp), max(end.slow$timestamp)), origin="1970-01-01")
  user.names <- computeUserFeatures(end.slow)
  e.s.user <- as.matrix(user.names[[1]])
  user.names <- user.names[[2]]
  nrow(e.s.user)
  par(mfrow=c(1,3))
  pri <- princomp(e.s.user)
plot(pri, main="Scree plot of regular PCA\nfor early times around demonstration")
# prm <- princomp(b.a.user, covmat=MASS::cov.mcd(b.a.user))
# plot(prm, main="Scree plot of PCA with MCD\nfor early times around demonstration")
prc <- prcomp(e.s.user, scale=TRUE)
plot(prc, main="Scree plot of rotated PCA\nfor early times around demonstration")
pcr <- PcaHubert(e.s.user)
sort(abs(pcr@loadings[,1]))
screeplot(pcr, main="Scree plot of ROBPCA\nfor early times around demonstration")
names(which(abs(pcr@loadings[,1])>.01))
names(which(abs(pcr@loadings[,2])>.01))
# end.act
nrow(end.act)
sum(is.na(end.act$pid))
as.POSIXlt(c(min(end.act$timestamp), max(end.act$timestamp)), origin="1970-01-01")
user.names <- computeUserFeatures(end.act)
e.a.user <- as.matrix(scale(user.names[["features"]]))
user.names <- user.names[["users"]]
nrow(e.a.user)
par(mfrow=c(1,3))
pri <- princomp(e.a.user)
plot(pri, main="Scree plot of regular PCA\nfor early times around demonstration")
# prm <- princomp(b.a.user, covmat=MASS::cov.mcd(b.a.user))
# plot(prm, main="Scree plot of PCA with MCD\nfor early times around demonstration")
prc <- prcomp(e.a.user[-1], scale=TRUE)
plot(prc, main="Scree plot of rotated PCA\nfor early times around demonstration")
pcr <- PcaHubert(e.a.user[-1])
sort(abs(pcr@loadings[,1]))
screeplot(pcr, main="Scree plot of ROBPCA\nfor early times around demonstration")
names(which(abs(pcr@loadings[,1])>.01))
names(which(abs(pcr@loadings[,2])>.01))

# Testing clustering on the early slow data
library(fpc)
scores <- t(t(pri$scores[, 1:3])*pri$sdev[1:3])
hc <- hclust(dist(scores), method="complete")
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
par(mfrow=c(1,1))
plot(2:30,
     silhouette.res,
     type="l",
     main="Different cluster validation indices",
     xlab="Number of clusters",
     ylab="Rescaled values",
     ylim=c(min(c(silhouette.res, pearsongamma.res, dunn.res, dunn2.res, wb.ratio.res, ch.res, sindex.res)),
            max(c(silhouette.res, pearsongamma.res, dunn.res, dunn2.res, wb.ratio.res, ch.res, sindex.res))))
lines(2:30, pearsongamma.res, lty="dotted", col="blue")
lines(2:30, dunn.res, lty="dashed", col="dark green")
lines(2:30, dunn2.res, lty="dotdash", col="dark green")
lines(2:30, wb.ratio.res, lty="dashed", col="blue")
lines(2:30, ch.res, lty="twodash", col="red")
lines(2:30, sindex.res, lty="solid", col="brown")

# get the optimal number (where most of the indices say yes):
getClusterNumber <- function(measures){
  # get the optimal number of clusters by choosing the most supperted one.
  # measures is a list of vectors, preferably with same length
  mins <- unlist(sapply(measures, getKnee))
  return(as.numeric(names(which.max(table(mins)))))
}
clnum <- getClusterNumber(list(silhouette.res, pearsongamma.res, dunn.res, 
                               dunn2.res, wb.ratio.res, ch.res, sindex.res)) + 1

# check the clusters' properties:
e.s.user <- as.data.frame(e.s.user)
e.s.user$user <- user.names
cl.props <- merge(as.data.frame(e.s.user),
                  data.frame(user=e.s.user$user,
                             membership=cutree(hc, k=clnum)),
                  by="user", all=TRUE)
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
                     ))
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

rm(list=ls())
gc()
##################
#  Do the search #
##################

# load functions
library(igraph)
library(rrcov)
library(fpc)
source("functions/createReplyGraph.R")
source("functions/rankSearcher.R")
source("functions/computeUserFeatures2.R")
source("functions/getKnee.R")
source("functions/analyzeRoles.R")
source("functions/draw.cm.times.R")
# load data
pd <- read.csv("cm_ranks.csv")
pd <- pd[order(pd$timestamp),]

# compute
alldata <- NULL
#for(i in 1:(nrow(pd)%/%500)){
for(i in ((nrow(pd)%/%500)-1):1){
  alldata <- c(alldata, list(analyzeRoles(slice(pd, 1000,500,i))))
  if(!(i%%10)){
    cat(paste0("\n---", i, "@", Sys.time(), "---\n"))
    saveRDS(alldata, file="back_alldata.Rdata")
  }
}
save.image()
alldata <- alldata[2:length(alldata)]
## analyzing results
# number of posts, users, days, clusters in one period
posts.number <- sapply(lapply(alldata, "[[", "data"), nrow)
users.number <- sapply(lapply(alldata, "[[", "users"), nrow)
clust.number <- sapply(lapply(alldata, "[[", "clusters"), nrow)
thread.number <- sapply(lapply(lapply(alldata, "[[", "data"), function(d){d[is.na(d$pid),]}), nrow)
last.time <- as.POSIXlt(sapply(lapply(lapply(alldata, "[[", "data"), "[[", "timestamp"),function(v){v[length(v)]}), origin="1970-01-01")
first.time <- as.POSIXlt(sapply(lapply(lapply(alldata, "[[", "data"), "[[", "timestamp"),"[", 1), origin="1970-01-01")
days.number <- mapply(difftime, last.time, first.time, MoreArgs=list(units="days"))
HHI <- function(v){
  (sum((v/sum(v))^2)-(1/length(v)))/(1-1/(length(v)))
}
HHIs <- sapply(lapply(lapply(alldata, "[[", "clusters"), "[[", "Freq"), HHI)
# plot
png(file="basic_info.png", height=768, width=512)
par(mfrow    =c(3,2))
plot(last.time, posts.number,
     xlab="Date",
     ylab="Number of posts",
     main="a. Number of posts in each step",
     pch=".")
draw.cm.times()
plot(last.time, users.number,
     xlab="Date",
     ylab="Number of users",
     main="b. Number of users in each step",
     pch=".")
draw.cm.times()
plot(last.time, clust.number,
     xlab="Date",
     ylab="Number of clusters",
     main="c. Number of clusters in each step",
     pch=".")
draw.cm.times()
plot(last.time, thread.number,
     xlab="Date",
     ylab="Number of threads",
     main="d. Number of threads in each step",
     pch=".")
draw.cm.times()
# plot(posts.number, thread.number,
#      xlab="Posts",
#      ylab="Threads",
#      main="e. Correlation of posts\nand the number of threads",
#      pch=16)
# plot(users.number, clust.number,
#      xlab="Users",
#      ylab="Clusters",
#      main="f. Correlation of active users\nand the number of clusters",
#      pch=16)
HHIs <- sapply(lapply(lapply(alldata, "[[", "clusters"), "[[", "Freq"), HHI)
plot(last.time, HHIs,
     xlab="Date",
     ylab="Herfindahl index",
     main="g. Equality of cluster sizes",
     pch=".")
abline(h=mean(HHIs))
draw.cm.times()
plot(clust.number, HHIs,
     xlab="Clusters",
     ylab="Herfindahl index",
     main="h. Relation between the Herfindahl index\nand the number of clusters",
     pch=16)
dev.off()
plot(density(HHIs),
     main="Kernel density estimate for HHI")


############
# ANALYSIS #
############
library(igraph)
library(rrcov)
library(fpc)
source("functions/createReplyGraph.R")
source("functions/rankSearcher.R")
source("functions/computeUserFeatures2.R")
source("functions/getKnee.R")
source("functions/analyzeRoles.R")
source("functions/chooseSignificantChanges.R")
source("functions/draw.cm.times.R")
alldata <- readRDS("alldata3.RDS")
pd <- read.csv("cm_ranks.csv")
pd <- pd[order(pd$timestamp),]

# First two clusterings for exaple of comparsion:
c1 <- analyzeRoles(slice(pd, 1000,500,1))
c2 <- analyzeRoles(slice(pd, 1000,500,2))
c3 <- analyzeRoles(slice(pd, 1000,500,3))
# perform chi2 on membership:
mbrs <- merge(c2$users[c("user", "membership")],
              c3$users[c("user", "membership")],
              by="user",
              all=TRUE)
chisq.test(as.matrix(table(mbrs[2:3], useNA="always")))
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
bsg <- bin.signif(as.matrix(table(mbrs[2:3], useNA="always")))
signif.1.star <- which(bsg>.95, arr.ind=TRUE)
signif.2.star <- which(bsg>.995, arr.ind=TRUE)
signif.3.star <- which(bsg>.999, arr.ind=TRUE)

# test plotting technique
chs <- chooseSignificantChanges(alldata[2:3], "parent.indeg.mean.mean", signif.threshold=.001)
png(file="example_rolechanges")
plotSignificantChanges(chs, "Parents' mean indegree")
dev.off()

#####################
# overall landscape #
#####################
all.changes <- chooseSignificantChanges(alldata, "parent.indeg.mean.mean", signif.threshold=0.001)
png(file="overall_landscape.png", width=8000, height=800)
plotSignificantChanges(all.changes, "Parents' mean indegree")
draw.cm.times()
dev.off()

## Hmmm... try to catch demonstrations
# max(which(all.changes[["times"]]<as.POSIXlt("2008-04-30")))
png(file="2008_demo.png")
plotSignificantChanges(sapply(all.changes, "[", 200:230), ylab="Parents' average indegree")
draw.cm.times()
dev.off()




























cufdat <- (computeUserFeatures(slice(pd, 1000,500,1)))
alldata <- list(NULL)
for(i in 1:(nrow(pd)%/%500)){
  alldata <- c(alldata, computeUserFeatures(slice(pd, 1000,500,i)))
  if(!(i%%10)){
    cat(paste0("\n---", i, "@", Sys.time(), "---\n"))
  }
}
save.image()
load(".RData")
for(i in i:(nrow(pd)%/%500)){
  alldata <- c(alldata, computeUserFeatures(slice(pd, 1000,500,i)))
  if(!(i%%10)){
    cat(paste0("\n---", i, "@", Sys.time(), "---\n"))
  }
}
# restructure the alldata list:
alldata <- alldata[2:length(alldata)]
alldat <- NULL
for(i in 1:(length(alldata)/3)){
  alldat <- c(alldat, list(alldata[((i*3)-2):(i*3)]))
}
library(rrcov)
feat1 <- alldata[[2]][-(3:4)]
feat1[is.na(feat1[,])] <- 0
# selecting which variables are
for(varnames in 1:ncol(feat1)){
  if(!sd(feat1[,varnames])){
    print(varnames)
  }
}
feat1 <- as.matrix(feat1[-(24:27)])

prc <- prcomp(feat1, scale=TRUE)
pcr <- PcaHubert(feat1, k=23, alpha=1)
pri <- princomp(feat1)
prm <- princomp(feat1, covmat=MASS::cov.mcd(feat1))
# run PCA:
library(pcaPP)
