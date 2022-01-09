## Determine whether the matrix is invertible, calculate 
## the inverse matrix, and cache it.

## dm will judge the matrix passed into function A to determine whether
## it is an invertible matrix or not.
dm <- function(m) "matrix" %in% class(try(solve(m),silent=TRUE))

## makeCacheMatrix is used to create a special vector containing 4 
## functions and the environment in which its creation is contained.

makeCacheMatrix <- function(x = matrix()) {
  if(dm(x) == TRUE) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinver <- function(inver) m <<- inver
    getinver <- function() m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
  } else {
    print ("The matrix is not invertible, please re-enter an invertible matrix!")
  }
  
}

## cacheSolve is used to calculate the inverse matrix of the matrix created by
## function makeCacheMatrix. It will first determine whether the inverse matrix
## has been calculated, and if so, it will be directly retrieved from the
## cache total, otherwise it will be calculated.

cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
