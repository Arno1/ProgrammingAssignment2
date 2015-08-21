## The two functions below create a special object that stores a matrix and caches its inverse

## The following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invertedMatrix <- NULL
      set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
      }
      get <- function() x
      setinverse <- function(cacheSolve) invertedMatrix <<- cacheSolve
      getinverse <- function() invertedMatrix
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" returned by makeCacheMatrix above 
## ,but first checks to see if if the inverse has already been calculated. If so, it gets it from the cache 
## and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getinverse()
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  data <- x$get()
  invertedMatrix <- solve(data, ...)
  x$setinverse(invertedMatrix)
  invertedMatrix
}
