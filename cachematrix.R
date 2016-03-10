#makeCacheMatrix() : returns a list containing accessor methods to set and get the matrix and its inverse from cache
#cachSolve(): tries to get the inverse from cache, if not present computes the inverse, sets in cache and returns the inverse matrix.


#makeCacgeMatrix takes a matrix input
      #setter getter methods are defined to set(reset) and access the matrix
      #setter getter methods are defined to set and access the inverse of the matrix input.
makeCacheMatrix <- function(x = matrix()) {
      #holder for the inverse
      m <- NULL
      # setter function to set a new matrix and reset the inverse.
      setMatrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      getMatrix <- function() x
      setInverseInCache <- function(inverse) m <<- inverse
      getInverseInCache <- function() m
      list(set = setMatrix, get = getMatrix,
           setInverseInCache = setInverseInCache,
           getInverseInCache = getInverseInCache)
}


#cachSolve(): tries to get the inverse from cache, if not present computes the inverse, sets in cache and returns the inverse matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverseInCache()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get();
      m <- solve(data)
      x$setInverseInCache(m)
      m
}