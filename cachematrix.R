
  -## The following are the functions for calculating and caching 
    # the inverse of a matrix. Caching helps in reducing the
    # computation time of complex processes.
    
  +## Below are two functions that are used to create a special 
    # object that stores a square matrix and caches its inverse. 
 
  +## The following makeCacheMatrix function is a list containing 
    # a function to 
    # 1. Set the value of the inversible matrix
    # 2. Get the value of the matrix
    # 3. Set the value of the inverse
    # 4. Get the value of the inverse
  
  makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
      x <<- y
      mi <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(inverse) 
      mi <<- inverse
    getmatrixinverse <- function() mi
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
  }
  
  
  +## The following cacheSolve function calculates the inverse 
    # of the matrix created with the above function. It first
    # checks to see if the inverse ha already been calculated.
    # if so, it gets the mean from the cache and skips the 
    # computation. Otherwise it calcualtes the invese of the
    # data  and sets the value of the inverse in the cache via 
    # the setmatrixinverse function.
    
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