## Matrix Inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it directly. This script contains a pair of 
## functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  # create a setter function  which sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # create a getter function which returns the value of the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setmatinv <- function(inv) m <<- inv
  
  # get the inverse of the matrix
  getmatinv <- function() m
  
  # create matrix object
  list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # calls getter function to get the inverse of the matrix
  m <- x$getmatinv()
  
  # Check if the returned matrix is valid and return a valid inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if getter function returns invalid matrix, then compute the matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinv(m)
  m
}
