## The purpose of the functions is to invert a matrix through the solve function
## and store it as a cached variable so that it doesn't have to be calculated every time when the underlying variable
## does not change. This reduces the overall computational requirements.

## Arguments: x - invertible matrix
## Purpose: Create a 'special' matrix to cache it's inverse.
## 1. Get the matrix that is currently stored
## 2. Set the matrix if it doesn't exist
## 3. setInverse the inverse matrix if it doesn't exist
## 4. getInverse the inverse matrix currently stored
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the Inverse matrix and stores it if it hasn't been calculated already. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data of inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

i <- makeCacheMatrix(diag(10,4))
cacheSolve(i)
