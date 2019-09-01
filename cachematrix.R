## The following two function makeCacheMatrix and cacheSolve give a
## way to calculate and cache the inverse of a matrix in a new data structure

## makeCacheMatrix creates and returns a list that implements a way to
## store a matrix and its inverse in the same data structure.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve either calculates or retrieves the inverse of a matrix
## given a list that was created by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
