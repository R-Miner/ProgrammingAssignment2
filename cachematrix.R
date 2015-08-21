#@@ -1,10 +1,15 @@
  -## [Put comments here that describe what your functions do]
  +## Put comments here that give an overall description of what your
  +## functions do
  +
  +## Write a short comment describing this function
  
  makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
      x <<- y
      mi <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(inverse) mi <<- inverse
    getmatrixinverse <- function() mi
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
  }
  
  
  +## Write a short comment describing this function
    +
    cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      mi <- x$getmatrixinverse()
      if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
      }
      data <- x$get()
      mi <- solve(data, ...)
      x$setmatrixinverse(mi)
      mi
    }