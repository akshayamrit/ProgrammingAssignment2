## Creating a special "matrix" object and caching the inverse of that Matrix.

## This function creates the special "matrix" object that caches the matrix as well as its inverse.
## This function works only when a square matrix is passed as argument.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It returns the inverse from cache if it exists in cache.
cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
      
}
