
setwd("C:\\Users\\tajtia\\Documents\\szkdgzt")
source("C:\\Users\\tajtia\\Documents\\R\\R-3.0.2\\etc\\i386\\Rprofile")
library(igraph)
edges <- read.csv("cm_ranks.csv")
edges <- edges[order(edges$timestamp),]
# activity over time:
plot(as.POSIXlt(edges[is.na(edges$p.uid), "timestamp"], origin="1970-01-01"),
     1:sum(is.na(edges$p.uid)),
     main="Number of posts over time",
     xlab="Time",
     ylab="Number of posts",
     pch=".")
# draw lines for criticalmass demostrations
abline(v=as.numeric(as.POSIXlt(c(""))))

# reaction times
react.dd <- density(edges[!is.na(edges$p.uid), "pid.react"])
## DENSITY-NEK VAN X és Y koordinátája, csekkold!
plot(density(edges[!is.na(edges$p.uid), "pid.react"]),
     log="xy",
     xlab="Reaction time",
     main="Distribution of reaction times")

# users lifetime: time between its firs and last comment
usr.lftm <- merge(
              aggregate(edges["timestamp"], as.list(edges["user"]), min),
              aggregate(edges["timestamp"], as.list(edges["user"]), max),
              by="user")
names(usr.lftm) <- c("user", "life.start", "life.end")
usr.lftm$range <- usr.lftm$life.end - usr.lftm$life.start
max(usr.lftm$range)/(365.25*24*3600)
mean(usr.lftm$range)/(365.25*24*3600)
# skewness:
sum((usr.lftm$range/(365.25*24*3600))>6)
sum((usr.lftm$range/(365.25*24*3600))>5)
sum((usr.lftm$range/(365.25*24*3600))>4)
sum((usr.lftm$range/(365.25*24*3600))>3)
sum((usr.lftm$range/(365.25*24*3600))>2)
sum((usr.lftm$range/(365.25*24*3600))>1)
sum((usr.lftm$range/(365.25*24*3600))==0)
# cut for days:
usr.lftm$day.range <- usr.lftm$range %/% (24*3600)
dt <- table(usr.lftm$day.range)
plot(names(dt),
     as.numeric(dt/sum(dt)),
     pch=16,
     log="xy",
     xlab="Lifetime (days)",
     ylab="Probability of the lifetime",
     main="Lifetime distribution among active users")

#user-user graph
cmUG <- graph.data.frame(edges[!is.na(edges$p.uid),
                              c("user", "p.uid", "id", "pid.react")])
E(cmUG)$weight=1
# basic measurements:
summary(cmUG)
cmCL <- clusters(cmUG)
graph.density(cmUG)
transitivity(cmUG)
diameter(cmUG)
# from here, largest component is used:
lwc <- induced.subgraph(cmUG, vids=which(cmCL$membership==1))
lwcCL <- clusters(lwc, "strong")
# SCC for further analysis
scc <- induced.subgraph(lwc, vids=which(lwcCL$membership== which.max(lwcCL$csize)))
# degree distribution:
scc.dd <- degree.distribution(scc)
# power laws:
plf <- power.law.fit(scc.dd)
plot(scc.dd, log="xy",
     main="Degree distribution of users' graph",
     xlab="degree (k)",
     ylab="P(X=k)")
lines(seq(scc.dd), seq(scc.dd) ^-plf$alpha)
# TODO: further investigation needed

# strength of connections:
# number of edges and mean time difference between users
cmUGs <- simplify(scc, edge.attr.comb=list(pid.react="mean",
                                           weight="sum"))
# connections between users:
u.cons <- E(cmUGs)$weight
u.cons.distr <- as.numeric(table(u.cons)/sum(u.cons))
plf.u.cons <- power.law.fit(u.cons.distr)
plot(u.cons.distr, log="xy",
     main="Multiplicity of edges in users' graph",
     xlab="Multiplicity",
     ylab="P(X=Multiplicity)")
lines(seq(u.cons.distr), seq(u.cons.distr) ^-plf.u.cons$alpha)

# mean time difference between users
plot(density(E(cmUGs)$pid.react),
     log="xy")

