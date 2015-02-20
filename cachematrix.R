## These functions are used to cache the inverse of matricies, ensuring that
## the inverse need only be caluclated once

## Creates a special matrix object that can cache the value of its mean
## when calculated

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL # cached inverse

   # Sets a new value for the matrix
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   # Retrieves the stored matrix value
   get <- function() x
   
   # Sets a cached value for the matrix inverse
   setinv <- function(sol) inv <<- sol

   # Retrieves the cached value of the matrix inverse
   getinv <- function() inv

   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv
}


## calculate the inverse of the cacheMatrix x. Will use the cached value
## instead of recalculating if invoked repeatedly 

cacheSolve <- function(x, ...) {
	   # changes in arguments other than x are beyond the scope of
	   # this assignment
	   i <- x$getinv()
   	   if (!is.null(i)) {
	      message("getting cached data")
	      return(i)
	   }

	   # no cached value, so must be calculated
	   data <- x$get()
	   i <- solve(x, ...)
	   x$setinv(i)
	   i
}
