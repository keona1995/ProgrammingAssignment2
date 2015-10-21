## makeCacheMatrix stores a list of functions and uses lexical scoping to
## maintain state across multiple calls to the internal functions. The purpose is to
## simulate a scenario where a long-running and presumably expensive calculation 
## should be executed only when necessary by caching the result and allowing queries of the
## result at any time.

## makeCacheMatrix is the principal function that defines a list of functions and stores
## the input matrix and the result of the calculation.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinverse <- function() inv
  setinverse <- function(calc) inv <<- calc
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve takes as input a makeCacheMatrix and returns the result of the 
## calculation of the inverse of a matrix. The actions the function takes
## depends upon the internal state of the makeCacheMatrix variable. If the result
## has already been calculated, it is returned immediately. If the result has not been
## calculated yet, it is kicked off.

cacheSolve <- function(x, ...) {
  result <- x$getinverse()
  if(!is.null(result)) {
    return(result)
  }
  mat <- x$get()
  tinv <- solve(mat)
  x$setinverse(tinv)
}
