## These functions create a list of functions that operate on a matrix
## To reduce processing time, an inverted matrix calculated from a data 
## set can be stored/cached for future retrieval

## makeCacheMatrix
## Can store a calulated inversion of a matrix for future retrieval
## setting a new value for the stored matrix/data will clear out the cached value
## returns a list of functions for operating on the stored matrix

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  
  ## when setting new values, clear cached value
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverted <<- inv
  getinverse <- function() inverted
  
  ## return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Tries to retrieve a previously calculated solution for the matrix
## If no solution currently stored then calculates the value and stores
## it for future retrieval

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if (!is.null(s)) {
      ## if a calculated value is found then prints out a message and returns the value
      message("getting cached data")
      return(s)
      ## exits the function
  }
  ## no stored value was found so calculates a new one and stores it for next time
  data <- x$get()
  s <- solve(a=data, ...)
  x$setinverse(s)
  s
}
