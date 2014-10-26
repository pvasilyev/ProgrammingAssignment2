## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setCachedInverted <- function(inv) inverted <<- inv
  getCachedInverted <- function() inverted
  list(set = set, get = get,
       setCachedInverted = setCachedInverted,
       getCachedInverted = getCachedInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getCachedInverted()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$setCachedInverted(inverted)
  inverted
}
