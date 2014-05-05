computeUserFeatures <- function(data, useReplyGraph=FALSE, mask=TRUE, pb=FALSE){

  # creating temporal ranks (full ranks are available in the full dataset)
  # calculate raw features
  dimnames(data)[[1]] <- data$id
  # create db for rank-based measures
  data <- data[order(data[, "user"], data[, "timestamp"]),]
  data$user.outdeg.local <- sequence(rle(data[["user"]])$lengths)
  # parent post's indegree
  data <- data[order(data[, "pid"], data[, "timestamp"]),]
  data$pid.indeg.local <- sequence(rle(data[["pid"]])$lengths)
  # parent node's indegree
  data <- data[order(data[, "nid"], data[, "timestamp"]),]
  data$nid.indeg.local <- sequence(rle(data[["nid"]])$lengths)
  # parent user's indegree
  data <- data[order(data[, "p.uid"], data[, "timestamp"]),]
  data$puid.indeg.local <- sequence(rle(data[["p.uid"]])$lengths)
  # user's and parent user's in- and outdegree respectively
  data$user.indeg.local <- mapply(function(user, parent, limit){
                                   sum(parent[1:limit] == user)
                                 },
                                 user=data$user,
                                 limit=1:nrow(data),
                                 MoreArgs=list(parent=data$p.uid))
  data$puid.outdeg.local <- mapply(function(user, parent, limit){
                                   sum(parent[1:limit] == user)
                                 },
                                 user=data$p.uid,
                                 limit=1:nrow(data),
                                 MoreArgs=list(parent=data$user))
  # check if it is a conversation
  data$conversation.local <- data[data$pid, "p.uid"] == data$user
  # rename the full ranks:
  names.of.data <- names(data)
  for(i in 1:length(data)){
    if(grepl("rank",names.of.data[i])){
      names(data)[i] <- paste0(names.of.data[i], ".full")
    }   
  }
  # compute new ranks
  data[["user.outdeg.rank"]] <- NA
  data[["user.indeg.rank"]] <- NA
  data[["pid.indeg.rank"]] <- NA
  data[["nid.indeg.rank"]] <- NA
  data[["puid.indeg.rank"]] <- NA
  data[["puid.outdeg.rank"]] <- NA
  #store the maximum rank:
  data[["user.outdeg.rank.max"]] <- NA
  data[["user.indeg.rank.max"]] <- NA
  data[["pid.indeg.rank.max"]] <- NA
  data[["nid.indeg.rank.max"]] <- NA
  data[["puid.indeg.rank.max"]] <- NA
  data[["puid.outdeg.rank.max"]] <- NA
  # data <- as.data.frame(rankSearcher(data))

  # difference between user's and parent's in- and outdegree:
  # data$local.indeg.diff <- data$user.indeg.local - data$puid.indeg.local
  # data$local.outdeg.diff <- data$user.outdeg.local - data$puid.outdeg.local

  # user's and parent's extroversion at the time (outdegree/indegree)
  # data$local.user.extrov <- data$user.outdeg.local/data$user.indeg.local
  # data$local.puid.extrov <- data$puid.outdeg.local/data$puid.indeg.local

  # relative rank
  # data$local.user.outdeg.rank.rel <- data$user.outdeg.rank/data$user.outdeg.rank.max
  # data$local.user.indeg.rank.rel <- data$user.indeg.rank/data$user.indeg.rank.max
  # data$local.pid.indeg.rank.rel <- data$pid.indeg.rank/data$pid.indeg.rank.max
  # data$local.nid.indeg.rank.rel <- data$nid.indeg.rank/data$nid.indeg.rank.max
  # data$local.puid.indeg.rank.rel <- data$puid.indeg.rank/data$puid.indeg.rank.max
  # data$local.puid.outdeg.rank.rel <- data$puid.outdeg.rank/data$puid.outdeg.rank.max

  # local.rank.table
  local.rank.table <- data.frame(user=unique(data$user))
  local.rank.table <- Reduce(function(x,y){
                          merge(x, y, by.x="user", by.y="Var1", all=TRUE)
                        },
                        list(local.rank.table,
                          as.data.frame(table(data$"user")),
                          as.data.frame(table(data$"p.uid"))))
  names(local.rank.table) <- c("user", "outdeg", "indeg")
  local.rank.table[is.na(local.rank.table[,])] <- 0
  local.rank.table$outdeg.rank <- rank(local.rank.table$outdeg)/max(rank(local.rank.table$outdeg))
  local.rank.table$indeg.rank <- rank(local.rank.table$indeg)/max(rank(local.rank.table$indeg))
  # local rank of posts
  post.lrt <- data.frame(id = data$id)
  post.lrt <- merge(post.lrt,
                    as.data.frame(table(data$"pid")),
                    by.x="id",
                    by.y="Var1",
                    all=TRUE)
  names(post.lrt) <- c("id", "indeg")
  post.lrt$indeg[is.na(post.lrt$indeg)] <- 0
  post.lrt$inrank <- rank(post.lrt$indeg)/max(rank(post.lrt$indeg))
  # threads' rank
  node.lrt <- data.frame(id = unique(data$nid))
  node.lrt <- merge(node.lrt,
                    as.data.frame(table(data$"nid")-1),
                    by.x="id",
                    by.y="Var1",
                    all=TRUE)
  names(node.lrt) <- c("id", "indeg")
  node.lrt$indeg[is.na(node.lrt$indeg)] <- 0
  node.lrt$inrank <- rank(node.lrt$indeg)



  # root posts
  init.rows <- which(is.na(data$p.uid))
  # reply graph
  if(useReplyGraph){
    from_to_nid <- data.frame(from=data$user,
                              to=data$user[sapply(data$pid, function(pid){
                                                    which(data$id==pid)
                                                  })],
                              nid=data$nid)
    simple.graph <- createReplyGraph(from_to_nid[complete.cases(from_to_nid),])
  } else {
    suppressWarnings(
      simple.graph <- graph.data.frame(unique(data[,
        c("user", "p.uid")])))
    simple.graph <- simple.graph-"NA"
  }
  
  graph.size <- vcount(simple.graph)

  # getting size of egocentric networks to partition users
  ego.size <- rep(NA, graph.size)
  # percent of bidirectional edges
  ego.reciprocity <- rep(NA, graph.size)
  ego.transitivity <- rep(NA, graph.size)
  # neigborhood in- and outdegree distribution exponent
  # nei.indeg.exp <- rep(list(rep(list(NA),6)), graph.size)
  # nei.outdeg.exp <- rep(list(rep(list(NA),6)), graph.size)
  # post/thread
  post.p.thread <- rep(NA, graph.size)
  nr.posts <- length(unique(data$nid))
  # initiated percent:
  init.perc <- rep(NA, graph.size)
  # initiations which got replied
  inits.nr <- rep(NA, graph.size)
  replied.inits <- rep(NA, graph.size)
  # replied posts:
  replied.posts <- rep(NA, graph.size)
  posts.nr <- rep(NA, graph.size)
  # inneighbours / all who has replied to else(=all-inneis?)
  indeg.percent <- rep(NA, graph.size)
  # forum entropy
  forum.entropy <- rep(NA, graph.size)
  # post/thread std
  std <- rep(NA, graph.size)
  # mean and deviance of serial number of comments at original posts
  serial.mean <- rep(NA, graph.size)
  serial.std <- rep(NA, graph.size)
  # mean and deviance of distance from root:
  dist.mean <- rep(NA, graph.size)
  dist.std <- rep(NA, graph.size)
  # mean and deviance of parent users in- and outdegree
  parent.indeg.mean <- rep(NA, graph.size)
  parent.indeg.std <- rep(NA, graph.size)
  parent.outdeg.mean <- rep(NA, graph.size)
  parent.outdeg.std <- rep(NA, graph.size)
  parent.indeg.rank.mean <- rep(NA, graph.size)
  parent.indeg.rank.std <- rep(NA, graph.size)
  parent.outdeg.rank.mean <- rep(NA, graph.size)
  parent.outdeg.rank.std <- rep(NA, graph.size)
  # enthropy of parent users
  parent.entropy <- rep(NA, graph.size)
  # mean and deviance of post properties
  post.indeg.mean <- rep(NA, graph.size)
  post.indeg.std <- rep(NA, graph.size)
  post.indeg.rank.mean <- rep(NA, graph.size)
  post.indeg.rank.std <- rep(NA, graph.size)
  # mean and deviance of root post properties
  node.indeg.mean <- rep(NA, graph.size)
  node.indeg.std <- rep(NA, graph.size)
  node.indeg.rank.mean <- rep(NA, graph.size)
  node.indeg.rank.std <- rep(NA, graph.size)
  
  
  # compute features for each user
  if(pb){
    cat("\nComputing original features for users...\n")
    prog.bar <- txtProgressBar(min=1, max=graph.size, style=3)
  }
  user.name <- V(simple.graph)$name
  for(i in 1:graph.size){
    user <- user.name[i]
    d <- data[which(data$user==user | data$p.uid==user),]

    G <- induced.subgraph(simple.graph,
      unlist(neighborhood(simple.graph,
        order=1, nodes=V(simple.graph)[i])),
      impl="copy_and_delete")
    inneis <- V(simple.graph)$name[unlist(
        neighborhood(simple.graph, 1, V(simple.graph)[i], mode="in"))]
    outneis <- V(simple.graph)$name[unlist(
        neighborhood(simple.graph, 1, V(simple.graph)[i], mode="out"))]
    ego.reciprocity[i] <- length(intersect(
      inneis, outneis))/vcount(G)

    G2 <- induced.subgraph(simple.graph,
      unlist(neighborhood(simple.graph,
        order=2, nodes=V(simple.graph)[i])),
      impl="copy_and_delete")

    # nei.indeg.exp[[i]] <- power.law.fit(degree(G2,
    #   v=V(simple.graph)$name[unlist(
    #     neighborhood(simple.graph, 1, V(simple.graph)[i], mode="in"))],
    #   mode="in"))
    # # cat(paste0(max(degree(G, mode="in")), "\n"))
    # nei.outdeg.exp[[i]] <- power.law.fit(degree(G2,
    #   v=V(simple.graph)$name[unlist(
    #     neighborhood(simple.graph, 1, V(simple.graph)[i], mode="out"))],
    #   mode="in"))
    ego.size[i] <- vcount(G)
    ego.transitivity[i] <- transitivity(G)
    # posts per thread
    id <- as.numeric(V(simple.graph)$name[i])
    post.p.thread[i] <- sum(data$user == id)/nr.posts
    # initiated percent:
    init.perc[i] <- sum(data$user[init.rows] == id)/
      nr.posts
    # replied inits percent
    user.inits <- data$id[intersect(init.rows,
      which(data$user == id))]
    inits.nr[i] <- length(user.inits)
    replied.inits[i] <- sum(user.inits %in% data$nid[-init.rows])/
                           length(user.inits)
    # replied posts
    user.posts <- data$id[data$user == id]
    posts.nr[i] <- length(user.posts)
    replied.posts[i] <- sum(user.posts %in% data$pid)/(length(user.posts))
    # innei/all others
    neis <- neighborhood.size(simple.graph, 1, V(simple.graph)[i], "in")
    indeg.percent[i] <- neis/graph.size
    # forum entropy
    post.distrib <- as.vector(table(data$nid[which(data$user==id)]))
    forum.entropy[i] <- -sum((post.distrib/length(user.posts))*
                            log(post.distrib/length(user.posts)))
    # post/thread std
    std[i] <- sd(post.distrib)
    # mean and deviance of serial number of comments at original posts
    serial.mean[i] <- mean(data[data$user==user,"node.s"])
    serial.std[i] <- sd(data[data$user==user,"node.s"])
    # mean and std of distance from original post
    dist.mean[i] <- mean(data[data$user==user,"node.dist"])
    dist.std[i] <- sd(data[data$user==user,"node.dist"])

    # parent enthropy
    parents.users <- data[data$user==user & !is.na(data$p.uid),"p.uid"]
    parent.entropy[i] <- -sum(table(parents.users)/length(parents.users)*
                               log(table(parents.users)/length(parents.users)))

    parent.indeg.mean[i] <- mean(local.rank.table[local.rank.table$user %in%
                                                    parents.users, "indeg"])
    parent.indeg.std[i] <- sd(local.rank.table[local.rank.table$user %in%
                                                 parents.users, "indeg"])
    parent.outdeg.mean[i] <- mean(local.rank.table[local.rank.table$user %in%
                                                    parents.users, "outdeg"])
    parent.outdeg.std[i] <- sd(local.rank.table[local.rank.table$user %in%
                                                 parents.users, "outdeg"])
    parent.indeg.rank.mean[i] <- mean(local.rank.table[local.rank.table$user %in%
                                                    parents.users, "indeg"])
    parent.indeg.rank.std[i] <- sd(local.rank.table[local.rank.table$user %in%
                                                 parents.users, "indeg"])
    parent.outdeg.rank.mean[i] <- mean(local.rank.table[local.rank.table$user %in%
                                                    parents.users, "outdeg"])
    parent.outdeg.rank.std[i] <- sd(local.rank.table[local.rank.table$user %in%
                                                 parents.users, "outdeg"])
    # post ranks
    post.indeg.mean[i] <- mean(post.lrt[which(post.lrt$id %in% data[data$user == user, "id"]),
                                     "indeg"])
    post.indeg.std[i] <- sd(post.lrt[which(post.lrt$id %in% data[data$user == user, "id"]),
                                  "indeg"])
    post.indeg.rank.mean[i] <- mean(post.lrt[which(post.lrt$id %in% data[data$user == user, "id"]),
                                     "inrank"])
    post.indeg.rank.std[i] <- sd(post.lrt[which(post.lrt$id %in% data[data$user == user, "id"]),
                                  "inrank"])
    # node.ranks
    node.indeg.mean[i] <- mean(node.lrt[which(node.lrt$id %in% data[data$user == user, "id"]),
                                     "indeg"])
    node.indeg.std[i] <- sd(node.lrt[which(node.lrt$id %in% data[data$user == user, "id"]),
                                  "indeg"])
    node.indeg.rank.mean[i] <- mean(node.lrt[which(node.lrt$id %in% data[data$user == user, "id"]),
                                     "inrank"])
    node.indeg.rank.std[i] <- sd(node.lrt[which(node.lrt$id %in% data[data$user == user, "id"]),
                                  "inrank"])
    
    if(pb){
      setTxtProgressBar(prog.bar, i)
    }
  }
  if(pb & !(i%%100)){
    close(prog.bar)
  }
  # create df
  features <- data.frame(size=ego.size,
                         indegree.percentage=indeg.percent,
                         # indegree.exponent=sapply(nei.indeg.exp,"[[","alpha"),
                         # outdegree.exponent=sapply(nei.outdeg.exp,"[[","alpha"),
                         reciprocity=ego.reciprocity,
                         transitivity=ego.transitivity,
                         posts=posts.nr,
                         replied.posts=replied.posts,
                         post.p.thread.sd=std,
                         post.p.thread=post.p.thread,
                         forum.entropy=forum.entropy,
                         initials=inits.nr,
                         initials.percentage=init.perc,
                         replied.initials=replied.inits,
                         serial.mean = serial.mean,
                         serial.sd = serial.std,
                         dist.mean = dist.mean,
                         dist.sd = dist.std,
                         user.indeg = local.rank.table$indeg,
                         user.indeg.rank = local.rank.table$indeg.rank,
                         user.outdeg.rank = local.rank.table$outdeg.rank,
                         parent.entropy = parent.entropy,
                         parent.indeg.mean = parent.indeg.mean,
                         parent.indeg.std = parent.indeg.std,
                         parent.indeg.rank.mean = parent.indeg.rank.mean,
                         parent.indeg.rank.std = parent.indeg.rank.std,
                         parent.outdeg.mean = parent.outdeg.mean,
                         parent.outdeg.std = parent.outdeg.std,
                         parent.outdeg.rank.mean = parent.outdeg.rank.mean,
                         parent.outdeg.rank.std = parent.outdeg.rank.std,
                         node.indeg.mean = node.indeg.mean,
                         node.indeg.std = node.indeg.std,
                         node.indeg.rank.mean = node.indeg.rank.mean,
                         node.indeg.rank.std = node.indeg.rank.std,
                         post.indeg.mean = post.indeg.mean,
                         post.indeg.std = post.indeg.std,
                         post.indeg.rank.mean = post.indeg.rank.mean,
                         post.indeg.rank.std = post.indeg.rank.std,
                         # network features
                         betweenness = betweenness(simple.graph, normalized=TRUE),
                         closeness = closeness(simple.graph, normalized=TRUE),
                         degree=degree(simple.graph),
                         evcent=evcent(simple.graph, directed=TRUE)$vector,
                         authority=authority.score(simple.graph)$vector,
                         hub.score=hub.score(simple.graph)$vector)
                         #page.rank=page.rank(simple.graph)$vector)

  # NA from Infs and NaNs
  if(mask){
    for(col in 1:NCOL(features)){
      features[[col]][!is.finite(features[[col]])] <- 0
    }
  }

  return(list(features=features,
              users=user.name))
}

computeUserFeatures <- compiler::cmpfun(computeUserFeatures)
