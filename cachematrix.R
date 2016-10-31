## Course : Coursera - R Programming - Programming Assignment 2
## Author: Sudhakar

### Function Descriptions ###
## makeCacheMatrix - function creates a matrix and sets inverse calculation in cache
## cacheSolve - function computes inverse of matrix

## function makeCacheMatrix creates a special matrix object and can cache its inverse
## and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {

  invMtx <- NULL
  # get function to get Matrix
  get <- function() {
    x
  }
  
  # set function to set a new Matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    invMtx <<- NULL
  }
  # getInverse to get InverseMatrix
  getInverse <- function() {
    invMtx
  }
  # setInverse with Inverse Matrix solution
  setInverse <- function(solvedMatrix) {
    invMtx <<- solvedMatrix
  }
  #return a list of functions
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
  
}


## function cacheSolve computes the inverse of the special "matrix" created by 
## function makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # querying and asssigning inverseMatrix cached value
  invMtx <- x$getInverse()
  
  # checking value of cache and using it if not NULL
  if(!is.null(invMtx)) {
    message("getting cached Inverse Matrix data ....")
    return(invMtx)
  }
  
  # gettting matrix and calculating inverse
  data <- x$get()
  invMtx <- solve(data, ...)
  # set value on inverse matrix in cache
  x$setInverse(invMtx)
  
  ## Return a matrix that is the inverse of 'x'
  return(invMtx)
  
}
