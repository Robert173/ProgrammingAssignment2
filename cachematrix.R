## These functions cache the inverse of a matrix so that it can
## be called without R having to calculate it again.

## This function produces a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) inverse <<- invert
  getinvert <- function() inverse
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## This function produces a matrix which is the inverse of the matrix
## returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inverse <- x$getinvert()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    }
  invmat <- x$get()
  inverse <- solve(invmat, ...)
  x$setinvert(inverse)
  inverse
}
