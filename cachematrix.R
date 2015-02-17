## this R script file holds two functions for calculating a matrix inverse
## and cache the result for optimizing future calculations. It's
## specially relevenat when using really big matrices.


## makeCacheMatrix
## ===============
##
## creates a special "matrix" with 4 associated "pseudo-methods"
## get and set for getting and setting data
## getInverse and setInverse for getting and setting the inverse of the matrix data
makeCacheMatrix  <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(mat) m <<- mat
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
## ==========
##
## solves the inverse of a matrix and caches the result for
## subsequent calls to cachesolve
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve( x$get() )
  x$setInverse(m)
  m
}
