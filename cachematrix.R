## implementing objects like structures in R in using functions and environments
## first function intializes a matrix with getters and setters
## second function calculates its inverse if it not calculated already


## creates an object that contains a matrix with a set of setters and getters
## input - a row matrix (assumption - matrix is invertible)
## set function stores the matrix
## get function returns the matrix
## setinverse function stores the inverse of the matrix
## getinverese function return the inverser of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## use to find the inverse of a matrix
## input - makeCacheMatrix type object(vector)
## output - the inverse of the matrix of the object
## the inverse is only calculated if it not already available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
