## the task is to cache the inverse of a matrix
## to do that, the functions should be able to
## to get the matrix, the function should be makeCacheMatrix
## it is said that this function creates a
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

## the next function should be cacheSolve
## this function calculates the inverse that is 
## returned by makeCacheMatrix above.
## to retrieve the inverse of the cache, we need 
## to observe if the matrix has not been change after
## the inverse of the special "matrix" 
## object has been calculated. 

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