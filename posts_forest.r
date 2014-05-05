#szakdolgozat

setwd("C:\\Users\\tajtia\\Documents\\szkdgzt")
source("C:\\Users\\tajtia\\Documents\\R\\R-3.0.2\\etc\\i386\\Rprofile")
library(igraph)
edges <- read.csv("cm_ranks.csv")
cmG <- graph.data.frame(edges[c("id", "pid", "user", "timestamp", "nid", "node.s", "node.dist")])
# delet the NA root of the blogs
cmG <- cmG - V(cmG)$name[which.max(degree(cmG, mode="in"))]

# get indegree-distribution, distance-distribution, etc:
summary(cmG)
plot(degree.distribution(cmG),
     log="xy",
     main="Indegree distribution of posts",
     xlab="indegree (k)",
     ylab="probability (P(x=k))",
     pch=18)
     
# how many nodes have larger in-degree than a given k?
sum(degree(cmG, mode="in")>1)
sum(degree(cmG, mode="in")>10)
sum(degree(cmG, mode="in")>20)
sum(degree(cmG, mode="in")>50)
sum(degree(cmG, mode="in")>100)
sum(degree(cmG, mode="in")>200)
png(file="post_indeg.png")
cmG.dd <- as.numeric(table(degree(cmG, mode="in"))/ecount(cmG))
names(cmG.dd) <- names(table(degree(cmG)))
plot(names(cmG.dd),
     unname(cmG.dd),
     log="xy",
     main="Distribution of indegree in the post-post graph",
     xlab="Indegree (k)",
     ylab="P(X=k)",
     pch=16)
dev.off()
plf <- power.law.fit(cmG.dd)

par(mfrow=c(1,2))
plot(cmG.dd,
  log="xy",
  ylab="probability (P(x=k))",
  xlab="k",
  sub="Lines' colour: xmin of power.law.fit(). Blue for low, yellow for high values",
  main="Degree distribution and fitted power.law coefficients")
cols <- topo.colors(100)
alphas <- rep(NA, 100)
ks.signif <- logical(100)
loglik <- rep(NA, 100)
for(xmin in 1:100){
  plf <- power.law.fit(cmG.dd[xmin:200])
  if(plf$KS.p > 0.5){
    abline(a=0.5, b=-plf$alpha, col=cols[xmin],  lwd=2)
  }
  alphas[xmin] <- plf$alpha
  ks.signif[xmin] <- (plf$KS.p)
  loglik[xmin] <- plf$logLik
}
points(cmG.dd, pch=16)     
plot(alphas,
     col=c("grey", "black")[(ks.signif>0.05) + 1],
     main="Fitted alpha parameters of power law distributions",
     xlab="kmin for power.law.fit()",
     ylab="alpha",
     sub="Grey values are not significant at * level.")
     
plot(cmG.dd,
  log="xy",
  ylab="probability (P(x=k))",
  xlab="k",
  sub="Lines' colour: xmin of power.law.fit(). Blue for low, yellow for high values",
  main="Degree distribution and fitted power.law coefficients")
lines(3:7, (3:7)^(-power.law.fit(cmG.dd[3:7])$alpha))

plot(alphas,
     cex=ks.signif,
     col=topo.colors(788)[trunc(loglik+1)],
     pch=19,
     main="Alpha values for fitted power law distributions",
     ylab="alpha",
     xlab="xmin (k)",
     sub="Deeper colour = larger likelihood; size ~ K-S P")


# post's lifetime:
pst.lftm <- merge(
              aggregate(edges["timestamp"], as.list(edges["id"]), min),
              aggregate(edges["timestamp"], as.list(edges["pid"]), max),
              by.x="id",
              by.y="pid",
              all.x=TRUE)
names(pst.lftm) <- c("post", "life.start", "life.end")
pst.lftm[is.na(pst.lftm$life.end), "life.end"] <- 
         pst.lftm[is.na(pst.lftm$life.end), "life.start"]
pst.lftm$range <- pst.lftm$life.end - pst.lftm$life.start
max(pst.lftm$range)/(24*3600)
mean(pst.lftm$range)/(24*3600)
# skewness:
sum((pst.lftm$range/(365.25*24*3600))>6)
sum((pst.lftm$range/(365.25*24*3600))>5)
sum((pst.lftm$range/(365.25*24*3600))>4)
sum((pst.lftm$range/(365.25*24*3600))>3)
sum((pst.lftm$range/(365.25*24*3600))>2)
sum((pst.lftm$range/(365.25*24*3600))>1)
sum((pst.lftm$range/((365.25/2)*24*3600))>1)
sum((pst.lftm$range/(30*24*3600))>1)
sum((pst.lftm$range/(7*24*3600))>1)
sum((pst.lftm$range/(24*3600))>1)
sum((pst.lftm$range/(365.25*24*3600))==0)



## THREAD ANALYSIS
# distribution of thread lengths
# in posts
node.length <- table(edges$nid)
node.l.d <- table(node.length)/sum(table(node.length))
png(file = "thread_length.png", width=960)
par(mfrow=c(1,2))
# indegree of posts
cmG.dd <- table(degree(cmG, mode="in"))/ecount(cmG)
names(cmG.dd) <- names(table(degree(cmG)))
plot(names(cmG.dd),
     unname(cmG.dd),
     log="xy",
     main="a. Distribution of indegree in the post-post graph",
     xlab="Indegree (k)",
     ylab="P(X=k)",
     pch=16)
# length of threads
plot(names(node.l.d),
     as.numeric(node.l.d),
     log="xy",
     main="b. Number of posts in threads",
     xlab="Number of posts",
     ylab="Proportion of threads with a given length",
     pch=16)
abline(v=300)
dev.off()
plf <- power.law.fit(as.numeric(node.l.d))

# in time
thrd.lftm <- merge(
              aggregate(edges["timestamp"], as.list(edges["nid"]), min),
              aggregate(edges["timestamp"], as.list(edges["nid"]), max),
              by="nid")
names(thrd.lftm) <- c("nid", "life.start", "life.end")
thrd.lftm$range <- thrd.lftm$life.end - thrd.lftm$life.start
thrd.lftm$day.range <- thrd.lftm$range%/%(24*3600)
thrd.lftm$month <- as.POSIXlt(thrd.lftm$life.start, origin="1970-01-01")$mon
# time and post lifetime correlation
plot(aggregate(thrd.lftm$day.range, list(thrd.lftm$month), "mean")[[1]]+1,
     aggregate(thrd.lftm$day.range, list(thrd.lftm$month), "mean")[[2]])
plot(thrd.lftm$month+1,
     thrd.lftm$day.range)
summary(lm(month ~ day.range, thrd.lftm))


x <- table(thrd.lftm$day.range)
plot(names(x),
     as.numeric(x),
     log="xy",
     main="Thread's lifetime distribution",
     xlab="Time (days)",
     ylab="Proportion of threads with a given lifetime",
     pch=16)
plf <- power.law.fit(as.numeric(x))

# check node creation month and length correlation:
node.l.df <- merge(edges[is.na(edges$pid),c("nid", "timestamp")], as.data.frame(node.length),
                  by.x="nid", by.y="Var1")
node.l.df$timestamp <- as.POSIXlt(node.l.df$timestamp, origin="1970-01-01")$mon
# get the mean length:
node.l.month <- aggregate(node.l.df$Freq, by=list(node.l.df$timestamp$mon), "mean")
summary(lm(timestamp ~ Freq, node.l.df))
plot(node.l.month[[1]]+1, node.l.month[[2]],
     main="Root post lengths and their month",
     xlab="Month",
     ylab="Mean post length",
     pch=16)
plot(node.l.df$timestamp$mon+1, node.l.df$Freq,
     main="Root post lengths and their month",
     xlab="Month",
     ylab="Mean post length",
     pch=16)
# compare lifetime and length deviance:
lftm.d <- scale(thrd.lftm$day.range, center=FALSE)
leng.d <- scale(node.l.df$Freq, center=FALSE)
sd(lftm.d)/mean(lftm.d)
sd(leng.d)/mean(leng.d)



# plot activity:
activity <- as.POSIXlt(edges$timestamp,
                    format="%Y-%m-%d",
                    origin="1970-01-01")
activity.c <- as.character(format(activity, format="%Y-%m-%d"))
activity.t <- table(activity.c)
plot(as.POSIXlt(names(activity.t)),
     as.numeric(activity.t),
     main="Activity of the forum",
     ylab="Number of posts",
     xlab="Date",
     type="l")
draw.cm.times <- function(){     
  abline(v=as.numeric(as.POSIXlt("2013-04-20")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2012-04-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2011-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2011-04-30")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2010-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2010-04-24")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2009-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2009-04-19")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2008-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2008-04-20")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2007-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2007-04-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2006-04-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2005-09-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2005-04-22")), col="dark green", lty="dotdash")
  abline(v=as.numeric(as.POSIXlt("2004-09-22")), col="dark green", lty="dotdash")
}
draw.cm.times()

# number of new threads:
activity <- as.POSIXlt(edges[edges$id>500000, "timestamp"],
                    format="%Y-%m-%d",
                    origin="1970-01-01")
activity.c <- as.character(format(activity, format="%Y-%m-%d"))
activity.t <- table(activity.c)
plot(as.POSIXlt(names(activity.t)),
     as.numeric(activity.t),
     main="Activity of the forum",
     ylab="Number of threads",
     xlab="Date",
     type="l")   
draw.cm.times()

# mean length of topics per day:
par(mfrow=c(2,2))
tpc <- merge(edges[edges$id>500000, c("id", "timestamp")],
             as.data.frame(node.length, stringsAsFactors=FALSE),
             by.x="id",
             by.y="Var1")
tpc <- aggregate(tpc$Freq,
                 list(as.character(as.POSIXlt(tpc$timestamp%/%(24*3600)*(24*3600),
                                              origin="1970-01-01"))),
                 mean)
plot(as.POSIXlt(tpc[[1]]),
     tpc$x,
     xlab="Date",
     ylab="Mean thread length",
     main="Mean thread length per day",
     pch=16)
draw.cm.times()
# draw running means:
ts <- numeric(difftime(as.POSIXlt(max(tpc[[1]])),
                       as.POSIXlt(min(tpc[[1]])),
                       units="days"))
names(ts) <- as.POSIXlt(min(tpc[[1]]))+((0:(length(ts)-1))*24*3600)
for(i in 1:nrow(tpc)){
  ts[[tpc[i,1]]] <- tpc[i,2]
}
# running means for 1 week:
week.mean <- numeric(length(ts)-6)
for(i in 7:length(ts)){
  week.mean[i-6] <- sum(ts[(i-7):i])/7
}
two.week.mean <- numeric(length(ts)-13)
for(i in 14:length(ts)){
  two.week.mean[i-13] <- sum(ts[(i-14):i])/14
}
four.week.mean <- numeric(length(ts)-27)
for(i in 28:length(ts)){
  four.week.mean[i-27] <- sum(ts[(i-28):i])/28
}
# plot 1 week lines
plot(as.POSIXlt(tpc[[1]]),
     tpc$x,
     xlab="Date",
     ylab="Mean thread length",
     main="Mean thread length per week",
     pch=".")
draw.cm.times()
lines(as.POSIXlt(names(ts[7:length(ts)])),
      week.mean,
      lty="dashed",
      col="blue")
# plot 2 week lines
plot(as.POSIXlt(tpc[[1]]),
     tpc$x,
     xlab="Date",
     ylab="Mean thread length",
     main="Mean thread length per four weeks",
     pch=".")
draw.cm.times()
lines(as.POSIXlt(names(ts[14:length(ts)])),
      two.week.mean,
      lty="dashed",
      col="purple")
# plot 4 week lines
plot(as.POSIXlt(tpc[[1]]),
     tpc$x,
     xlab="Date",
     ylab="Mean thread length",
     main="Mean thread length per four weeks",
     pch=".")
draw.cm.times()
lines(as.POSIXlt(names(ts[28:length(ts)])),
      four.week.mean,
      lty="dashed",
      col="red")
            
     
# lifetime of topics on a given day
thrd.lftm$day.start <- as.character(as.POSIXlt((thrd.lftm$life.start%/%(24*3600))*(24*3600),
                                               origin="1970-01-01"))
# merging to count:
tpc <- aggregate(thrd.lftm$day.range, list(thrd.lftm$day.start), mean)
plot(as.POSIXlt(tpc[[1]]),
     tpc[[2]],
     main="Mean lifetime of threads",
     ylab="Mean lifetime",
     xlab="Date",
     pch=16)
lines(as.POSIXlt(tpc[[1]]),
      difftime(max(as.POSIXlt(tpc[[1]])), tpc[[1]], units="days"),
      col="red")
# relative lifetime to max. possible lifetime:
plot(as.POSIXlt(tpc[[1]]),
     tpc[[2]] / as.numeric(difftime(max(as.POSIXlt(tpc[[1]])),
                                    tpc[[1]],
                                    units="days")),
     main="Mean thread lifetime compared to the possible maximum",
     ylab="Relative lifetime",
     xlab="Date",
     pch=16)
draw.cm.times()

# what is the diameter and the density of this tree? Are they corresponding 
#   a BA-model? They could be generated by one...
dens <- graph.density(cmG)
diam <- diameter(cmG)
trans <- transitivity(cmG, "global")

# farthest points:
fp <- farthest.nodes(cmG)
# plot that tree:
membersof <- clusters(cmG)$membership[fp[1]]
sg <- induced.subgraph(cmG, vids=which(clusters(cmG)$membership == membersof))
# export edgelist to plot it with sthg else:
write.csv(get.edgelist(sg), file="E:\\szkdgzt\\largest_tree_edges.csv", row.names=FALSE)
# tkplot(as.undirected(sg),
       # layout=layout.reingold.tilford(sg, 
         # root=which(as.numeric(V(sg)$name) > 500000),
         # circular=TRUE))
# plot(sg, layout=layout.reingold.tilford(sg, 
                  # root=which(as.numeric(V(sg)$name) > 500000),
                  # circular=TRUE))


                  
                  
                  
                  
                  
                  
# generate a BA-model for each tree.:
sizes.of.trees <- clusters(cmG)$csize
G3 <- graph.empty()
for(gr.size in sizes.of.trees){
  G3 <- G3 %du% ba.game(gr.size, power=3, m=1)
}
save(G, file="refForest.igraph")
points(names(table(degree(G3, mode="in"))),
      unname(table(degree(G3, mode="in"))),
      log="xy",
      main="In-degree distribution of posts",
      xlab="in-degree",
      ylab="frequency",
      pch="°",
      col="red")
# power <- 3.5
G3.5 <- graph.empty()
for(gr.size in sizes.of.trees){
  G3.5 <- G3.5 %du% ba.game(gr.size, power=3.5, m=1)
}
save(G3.5, file="refForest3_5.igraph")
points(names(table(degree(G3.5, mode="in"))),
      unname(table(degree(G3.5, mode="in"))),
      log="xy",
      main="In-degree distribution of posts",
      xlab="in-degree",
      ylab="frequency",
      pch="°",
      col="blue")

# power <- 5
G5 <- graph.empty()
for(gr.size in sizes.of.trees){
  G5 <- G5 %du% ba.game(gr.size, power=5, m=1)
}
save(G5, file="refForest5.igraph")
points(names(table(degree(G5, mode="in"))),
      unname(table(degree(G5, mode="in"))),
      log="xy",
      main="In-degree distribution of posts",
      xlab="in-degree",
      ylab="frequency",
      pch="°",
      col="green")


# hmm... magától nem power-law, de nézzük csak xmin változtatásával:
KSp <- numeric(100)
KSfit <- numeric(100)
alpha <- numeric(100)
loglik <- numeric(100)

for(xmin in 1:100){
  (paste(xmin, "\n"))
  pwf <- power.law.fit(degree(cmG, mode="in"), xmin=xmin)
  KSp[xmin] <- pwf$KS.p
  KSfit[xmin] <- pwf$KS.stat
  alpha[xmin]<- pwf$alpha
  loglik[xmin] <- pwf$logLik
}
par.old <- par()
par(mfrow=c(2,2))
plot(KSp,
  xlab="Minima of in-degrees",
  ylab="Probability that the distribution is power-law",
  main="Significance of Kolgomorow-Smirnov test\n for in-degree data of posts",
  pch =".")
abline(h=0.05)
abline(v=c(18,76))

plot(KSfit,
  xlab="Minima of in-degrees",
  ylab="KS statistic",
  main="Fittness of the data",
  pch =".")
abline(h=0.05)
abline(v=c(18,76))

plot(alpha,
  xlab="Minima of in-degrees",
  ylab="Power of the power-law distribution",
  main="Power of the fitted power-law\ndistribution in-degree data of posts",
  pch =".")
abline(v=c(18,76))


plot(abs(loglik),
  xlab="Minima of in-degrees",
  ylab="log-likelihood that the power is the real one",
  main="Log-likelihood of the probability\nthat the alpha is OK.",
  pch =".")
abline(v=c(18,76))


par() <- par.old


# load the big database with features, and compute others:
cm.features <- read.csv("posts_feats.csv")
# adding n.uid to the DB:
nodes <- unique(cm.features[which(is.na(cm.features$pid)), c("id", "user", "timestamp")])
dimnames(nodes)[[1]] <- nodes$id
cm.features[["n.uid"]] <- nodes[cm.features[["nid"]], "user"]
# haw many nodes did te nuid already created?
nodes <- nodes[order(nodes[,"user"], nodes[,"id"]),]
nodes$nuid.serial <- sequence(rle(nodes[["user"]])$lengths)
cm.features[["nuids.nodes"]] <- nodes[as.character(cm.features[["nid"]]), "nuid.serial"]
# time of root post
cm.features[["nid.ts"]] <- nodes[as.character(cm.features[["nid"]]), "timestamp"]
# time of parent post
parents <- unique(cm.features[["pid"]])
Sys.time()
for(i in 1:length(parents)){
  cm.features[["parent.ts"]][which(is.true(cm.features$pid==parents[i]))] <- 
    cm.features[which(cm.features$id==parents[i]), "timestamp"]
  if(!(i%%1000)){
    cat(paste(i, "@", Sys.time(), "\n"))
  }
}

# write.csv(cm.features, file="cm_feats.csv", row.names=FALSE)

# compute features:
source("functions/computeUserFeatures.R")

# sample dataset:
sam.dat <- cm.features[order(cm.features$timestamp),][1:10000,]


