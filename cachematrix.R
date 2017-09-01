## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Make a matrix and set the invers value
makeCacheMatrix <- function(x = matrix(..., nrow, ncol, byrow = FALSE)) {
  ## No inversion conducted  
  inversi <- NULL
  ## If the value of the matrix changed,
  ## set new value to parent function and NULLify the inverse value
  set <- function(y) {
    x <<- y
    inversi <<- NULL
  }
  ## get the matrix values
  getMatrix <- function() x
  ## set the inverse matrix value
  setInvers <- function(inverseVal) inversi <- inverseVal
  ## get the inverse value
  getInvers <- function() inversi
  list(set = set, getMatrix = getMatrix,
       setInvers =setInvers, getInvers = getInvers)
}


## Write a short comment describing this function
## Calculating the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the inverse value from getInvers
  inversi <- x$getInvers()
  ## Check whether the value is NULL or not
  if(!is.null(inversi)){
    message("Getting cached data..")
    ## Return the value of inverse matrix
    inversi
  }
  ## If the inverse matrix value is NULL, then calculate it
  ## Get the matrix from getMatrix
  data <- x$getMatrix()
  ## Inversion process here
  ## Assume the matrix is square: YES
  ## inversi <- solve(data)
  ## Assume the matrix is square: NO
  ## load library MASS
  library(MASS)
  inversi <- ginv(data)
  ## Give the value to setInvers
  x$setInvers(inversi)
  ## return the value
  inversi
}