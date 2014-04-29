## This script defines two functions.  The first function creates an object
## contains a matrix and its corresponding inverse if it has been cached.
## The second function takes an object of the type described above and returns
## the inverse.

#Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(val) inv <<- val
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv= getInv)
}


# This function returns the cached inverse of a CacheMatrix or otherwise 
#computes it
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (is.null(inv)){
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInv(inv)
    inv
  }
  else{
    message("getting cached data")
    return(inv)
  }
  
}
