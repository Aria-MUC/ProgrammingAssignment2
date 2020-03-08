makeCacheMatrix <- function(x = matrix()) {
  s<-matrix()
  set<-function(y){
    x<<-y 
    s<<-matrix()
  }
  
  get<-function() x
  setM<-function() s<<-cacheSolve(x)
  getM<-function() s
  list(set=set,get=get,setM=setM,getM=getM)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getM()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setM(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
