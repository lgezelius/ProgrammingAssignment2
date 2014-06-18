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

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## This functions creates a special "matrix", which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache,
## otherwise it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
