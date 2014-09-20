## Two functions
## - makeCacheMatrix
## - cacheSolve
## are defined.
##
## The purpose of the 2 functions is to provide a mean to calculate
## the inverse of a given matrix and to cache that inverse.
## The cached inverse can be used in case there is a need to calculate
## the inverse again later. There is no need to
## execute the expensive inverse calculation again. Instead the cached
## inversed can be reused.

## Function: makeCacheMatrix
## Description:
## A function which provides an environment to cache a matrix and its
## inverse. The function returns a list of functions which allow to operate
## on the cached data.
## Arguments:
## x:               A matrix, which is assumed to be invertible.
##                  If omitted, the empty matrix is used
## Value:
## A list of functions is returned:
## set(y):          Sets a new value 'y' for the matrix.
## get():           Returns the cached matrix.
## setinverse(inv): Sets the value of the inverse of the matrix 'inv'.
## getinverse():    Returns the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Function: cacheSolve
## Description:
## A function which calculates the inverse of a matrix, which has been
## already given as argument to the previous function makeCacheMatrix.
## The function checks if an already cached inverse of the matrix exists.
## If yes, then it returns the cached inverse. 
## If no, it calculates the inverse, caches and returns it.
## Arguments:
## x:             The matrix to be inverted. It is assumed that 'x' is
##                invertible.
## ...:           Not used.
## Value:
## The inverse matrix of 'x'.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
