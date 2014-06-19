## A pair of functions that cache the inverse of a matrix.

## Example:
## > my_matrix = makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(my_matrix)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(my_matrix)
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## An alternative way to create and set a cacheMatrix is:
## > my_matrix = makeCacheMatrix()
## > my_matrix$set(rbind(c(1, -1/4), c(-1/4, 1)))

## makeCacheMatrix function creates an object that can cache the inverse of a matrix. 
## This functions creates an object, which is a list containing functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(m = matrix()) {
  ## m and i are variables bound to the cacheMatrix object.
  ## m is the matrix and i is the inverse matrix.
  ## The following are equivalent ways to create and set a cacheMatrix:
  ## > my_matrix = makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
  ## OR
  ## > my_matrix = makeCacheMatrix()
  ## > my_matrix$set(rbind(c(1, -1/4), c(-1/4, 1)))
  i <- NULL
  set <- function(y) {
    # set m and i in the parent environment (the makeCacheMatrix function)
    m <<- y
    i <<- NULL
  }
  # The get function returns the matrix
  get <- function() m
  # The setsolve function sets the inverse matrix and is intended to be called only by cacheSolve
  setsolve <- function(solve) i <<- solve
  # the getsolve function gets the inverse matrix and is intended to be called only by cacheSolve
  getsolve <- function() i
  # make the functions accessible using $
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the matrix stored in a cacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache,
## otherwise it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(cm, ...) {
  ## cm is the cacheMatrix and i is the inverse of the cacheMatrix matrix
  ## retrieve the inverse from cacheMatrix
  i <- cm$getsolve()
  ## if the inverse is not null, brag about caching and return the inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## otherwise calculate the inverse of the cm matrix, store the inverse in cm, and return the inverse
  m <- cm$get()
  i <- solve(m, ...)
  cm$setsolve(i)
  i
}
