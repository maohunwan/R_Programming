## Made two functions here: makeCacheMatrix and cacheSolve
## makeCacheMatrix aims to generate a list of functions with the environment of which
## the matrix values can be stored without being cleared.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the 2nd function "cacheSolve" is to utitlize the functions created by "makeCacheMatrix" and
## the matrix value stored in the environment and conduct the computation for getting 
## the inverse Matrix. 

cacheSolve <- function(x) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    datamatrix <- x$get()
    m <- solve(datamatrix)
    x$setinv(m)
    m
}
