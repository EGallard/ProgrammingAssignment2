
## The aim of this assignment is to calculate in an efficient way the inverse of a matrix
## Calculating the inverse of a matrix can be very expensive (in terms of computation), so it´s better to cache the inverse 
## instead of computing it each time.
## This is managed in the function makeCacheMatrix through the use of set, get, setInverse and getInverse,
## following the way it was done in the teacher's example
## with set, get, setmean and getmean.
## The function solve calculates the inverse of a matrix [solve(m)]
## (https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/solve)


makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function() inv <<- solve(m) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function, called cacheSolve, calculates the inverse of the matrix created  with makeCacheMatrix.
## As in the example given by the teacher (cachemean) it first checks if the inverse has already been calculated . 
## In that case [!is.null(inv] it gets the inverse from the cache [x$getInverse] and skips the computation
## In othe case, it  will calculate the inverse of the matrix, again with the use of solve.



cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'm'
    inv <- m$getInverse(),
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- m$get()
    inv <- solve(mat, ...)
    m$setInverse(inv)
    inv
}
