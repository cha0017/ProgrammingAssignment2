## the task is to cache the inverse of a matrix
## to do that, the functions should be able to
## cache its inverse and 
## create a special matrix object
##
## to get the matrix, the function creates a
## special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##
## the next function should be cacheSolve
## this fuction computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <-function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}