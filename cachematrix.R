## Utility functions for caching a matrix inverse,
## for faster computations (ex: big matrix and big loop)

# makeCacheMatrix
# Create a special "matrix", which is really a list
# containing a function to get and set the matrixand to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(y) inv <<- y
  list(get = get, set = set, getinv = getinv, setinv = setinv)
}

# cacheSolve
# Take a special "matrix" created with the above function
# check if the inverse matrix was already calculated
# return the cached matrix if so, or
# calculate the inverse, cache it and return it
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
