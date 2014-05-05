createReplyGraph <- function(from_to_thread){
  # Creates a reply graph used in Chan-Hayes:
  # Decomposing Sicussion Forums using User Roles
  #
  # Edge between users: reply from U1 to U2's post in a thread
  # Edge weight: number of replies from U1 to U2
  #
  # relies on igraph R package

  # aggregate edges by thread
  aggregated.edges <- aggregate(
    data.frame(weight=rep(1, NROW(from_to_thread))),
    by=from_to_thread,
    FUN=sum)

  # creating a graph from the edgelist
  reply.graph <- graph.data.frame(aggregated.edges)

  return(reply.graph)
}