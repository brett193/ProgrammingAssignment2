## This files contains two function that serve intertwined purposes.
## In an effort to reduce the execution time for calculations that
## require repeated use of a matrix's inverse, these functions 
## allow for the value to be cached after calculation and simply
## returned for every subsequent request.

## The makeCacheMatrix is a function that returns a special type
## of list.  This list enables the user to get and set a matrix
## value that can then be inverted and the result cached for 
## later re-use without recalculation.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse=setinverse,
       getinverse=getinverse)  
}

## The cacheSolve function, when passed a special
## 'matrix' created by the makeCacheMatrix function,
## will attempt to get the inverse of the matrix
## stored on the cacheMatrix.  If the inverse has
## not been calculated, it will calculate it and 
## save the result on the cacheMatrix for future
## use.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
