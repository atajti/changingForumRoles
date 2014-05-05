rank.searcher <- function(posts.data){
  # Function to compute posting ranks

  posts.data <- as.matrix(posts.data)
  for(i in nrow(posts.data):1){
    user <- posts.data[i, "user", drop=FALSE]
    parent <- posts.data[i, "p.uid", drop=FALSE]
    pid <- posts.data[i, "pid", drop=FALSE]
    nid <- posts.data[i, "nid", drop=FALSE]
    # creating db to compute rank measures
    users <- posts.data[1:i, c("user", "user.outdeg", "user.indeg"), drop=FALSE]
    parents <- posts.data[1:i, c("p.uid", "puid.outdeg", "puid.indeg"), drop=FALSE]
    pids <- posts.data[1:i, c("pid", "pid.indeg"), drop=FALSE]
    nids <- posts.data[1:i, c("nid", "nid.indeg"), drop=FALSE]
    # deleting non-last entries to rank only the updated data
    users <- users[which(!duplicated(users[, "user"], fromLast=TRUE)), , drop=FALSE]
    dimnames(users)[[1]] <- users[, "user"]
    parents <- parents[!duplicated(parents[, "p.uid"], fromLast=TRUE) &
                       !is.na(parents[, "p.uid"]), , drop=FALSE]
    dimnames(parents)[[1]] <- parents[, "p.uid"]
    pids <- pids[!duplicated(pids[, "pid"], fromLast=TRUE) &
                       !is.na(pids[, "pid"]), , drop=FALSE]
    dimnames(pids)[[1]] <- pids[, "pid"]
    nids <- nids[!duplicated(nids[, "nid"], fromLast=TRUE), , drop=FALSE]
    dimnames(nids)[[1]] <- nids[, "nid"]
    # computing the rank and insert the last element into the posts.data
    user.outdeg.rank <- rank(users[, "user.outdeg"], na.last=FALSE)
    # names(user.outdeg.rank)[[1]] <- dimnames(users)[[1]]
    posts.data[i, "user.outdeg.rank"] <- user.outdeg.rank[as.character(user)]
    posts.data[i, "user.outdeg.rank.max"] <- max(user.outdeg.rank)
    user.indeg.rank <- rank(users[, "user.indeg"], na.last=FALSE)
    # names(user.indeg.rank)[[1]] <- dimnames(users)[[1]]
    posts.data[i, "user.indeg.rank"] <- user.indeg.rank[as.character(user)]
    posts.data[i, "user.indeg.rank.max"] <- max(user.indeg.rank)
    nid.indeg.rank <- rank(nids[, "nid.indeg"], na.last=FALSE)
    # names(nid.indeg.rank)[[1]] <- dimnames(nids)[[1]]
    posts.data[i, "nid.indeg.rank"]<- nid.indeg.rank[as.character(nid)]
    posts.data[i, "nid.indeg.rank.max"]<- max(nid.indeg.rank)
    # if there's no parent post, these are NAs'
    if(!is.na(pid)){
      pid.indeg.rank <- rank(pids[, "pid.indeg"], na.last=FALSE)
      # names(pid.deg.rank) <- dimnames(pids)[[1]]
      posts.data[i, "pid.indeg.rank"] <- pid.indeg.rank[as.character(pid)]
      posts.data[i, "pid.indeg.rank.max"] <- max(pid.indeg.rank)
      puid.indeg.rank <- rank(parents[, "puid.indeg"], na.last=FALSE)
      # names(puid.indeg.rank)  <- dimnames(parents)[[1]]
      posts.data[i, "puid.indeg.rank"]<- puid.indeg.rank[as.character(parent)]
      posts.data[i, "puid.indeg.rank.max"]<- max(puid.indeg.rank)
      puid.outdeg.rank <- rank(parents[, "puid.outdeg"], na.last=FALSE)
      # names(puid.outdeg.rank)  <- dimnames(parents)[[1]]
      posts.data[i, "puid.outdeg.rank"]<- puid.outdeg.rank[as.character(parent)]
      posts.data[i, "puid.outdeg.rank.max"]<- max(puid.outdeg.rank)
    }
    if(!(i%%1000)){
      cat(paste(i, " @ ", Sys.time(), "\n"))
    }
  }
  return(posts.data)
}

# compiling it to be faster:
rankSearcher <- compiler::cmpfun(rank.searcher)
