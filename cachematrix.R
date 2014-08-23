## The following 2 functions are desinged to calculate and cache the inverse of a matrix.
## The first function caches the matrix and its inverse. 
## The second function retrieves the cached data, or calculates and caches the inverse of the matrix if there is no cached data.

## The following function, makecacheMatrix creates a special matrix object that are 
## actually a list containing a function of the following purposes:
## (1)set the value of the matrix
## (2)get the value of the matrix
## (3)set the value of the inverse of the matrix
## (4)get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## im is initiated for the inverse data
  im <- NULL
  
  ## set (cache) the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## get the cached matrix
  get <- function() x
  
  ## set (cache) the inverse data
  setinverse <- function(invs) im <<- invs
  
  ## get the inverse data
  getinverse <- function() im
  
  ## the function returns the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" object created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the cached inverse data
  invs <- x$getinverse()
  
  ## if there is valid cached data, return it
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  
  ## if the cached data is null, get the cached matrix into 'data' and calculate the inverse by function solve(), 
  ## then cache the inverse of the matrix by 'x$setinverse()' and return it.
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}
