## This is a program to find the inverse of a matrix and cache results so as to avoid rework.
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    # new value of matrix is assigned. flush cache. set to NULL
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) cache <<- solve
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
