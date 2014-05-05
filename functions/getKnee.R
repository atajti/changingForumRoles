getKnee <- function(vector, m=NA){
  # this function is used to search a peak of cluster validation measures.
  # vector: numeric vector of values for number of clusters
  # m: what is the good measure? minimum or maximum? 
  #    if NA, method will compare the last value with the mean.
  #    if the last value is larger than the mean, it is a max, else it's min.

  v <- as.numeric(vector)
  # shift everityng above 0
  if(any(v < 0)){
    v <- v + min(v) + 1
  }

  # minimum or maximum do we need?
  if(is.na(m)){
    if(mean(v)<v[length(v)]){
      m <- "max"
    } else {
      m <- "min"
    }
  }
  if(!(m %in% c("min", "max"))){
    stop("m must be either 'min' or 'max'!")
  }

  # if we want min, search the max in -1*v:
  if(m=="min"){
    v <- v*(-1)
  }

  # check for all value if it is higher than its neighbours:
  vm <- logical(length(v)-2)
  for(i in 2:(length(v)-1)){
    if((v[i] > v[i-1]) & (v[i] > v[i+1])){
      vm[i-1] <- TRUE
    } else {
      vm[i-1] <- FALSE
    }
  }
  # where are maxes?
  maxes <- which(vm)+1
  # if it is monotone, check for sharpest knees:
  if(!length(maxes)){
    maxes <- which.max(diff(diff(v)))+2
  }
  
  return(maxes)
}

getKnee <- compiler::cmpfun(getKnee)