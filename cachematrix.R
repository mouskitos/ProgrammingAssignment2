##Final version of assignment 2
##This R code contains 2 functions. 
##the function makeCacheMatrix that creates a Matrix object that can cache its inverse
##The function cacheSolve computes the inverse of the matrix created by MakeCacheMatrix.
##If the inverse has already been computed, then cacheSolve retrieve the value of the inverse from the cache


## makeCacheMatrix lists four functions which have the following goals:
##the function set() set the values of x in the cache and assign the value NULL to object Inv in the cache
##the function get() allows to get the values of x 
## the function setInverse() assings the value of the inverse of x to the object Inv in the cache
## The function getInverse() allows to get the values of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve will first assign the value of Inv using the function getInverse
## if the value of Inv is not null i.e. Inv is calculated, its value is got from cache
## if it is not the case i.e.  the inverse of x is calculated and assigned to Inv in cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  else { data <- x$get()
         Inverse <- solve(data, ...)
         x$setInverse(Inverse)
         Inv
  }
}
