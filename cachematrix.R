## BH 11/2014
##Put comments here that give an overall description of what your
## functions do

## BH: MakeCacheMatrix initialize the variables and function calls for the caching inverse matrix.
## Set and Get allows saving and retrieving cached values.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## BH 11/23/2014:  CACHESOLVE attempts to retreive a cached value of the inverse matrix.
## if there is no cached value, it is generated and saved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Below code generates the cached value for the first time
  data <- x$get()
  m <- solve(data
  x$setinverse(m)
  m
}
