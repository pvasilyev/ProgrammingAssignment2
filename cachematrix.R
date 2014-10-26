## Following functions can be used to create special wrapper over given matrix,
## which is able to cache invertion of this matrix, a.k.a. solve(matrix).
## So if the matrix is not changed - then the invertion computed only once and after that
## returned only cached result.
## When the data of the wrapper is changed - then cache is expired and invertion is re-computed.

## This function creates a caching-wrapper for a given matrix 'x'.

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


## This function computes the invertion for the wrapper if is hasn't computed so far.
## Otherwise it returns cached result for the 'x'.

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
