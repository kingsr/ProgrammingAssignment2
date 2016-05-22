## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                                  # sets m to NULL
  set <- function(y) {
    x <<- y                                  # sets x to the value of y in the (global env)
    m <<- NULL                               # sets m to NULL (global env)
  }
  get <- function() x

  setSolve <- function(solve) m <<- solve    # sets m to solve (global env)       
  getSolve <- function() m
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {             
  
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m                                          # Returns 'm' a matrix that is the inverse of 'x'
}
