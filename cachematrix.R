## Returns a list of functions that can be used to (1) set the value
## of the matrix, (2) get the value of the matrix, (3) set the value
## of the inverse, (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  ## the inverse is initially undefined
  set <- function(y) {  ## function that sets the value of the matrix
    x <<- y
    inverse <<- NULL  ## need to unset the cache if we're changing the matrix
  }
  get <- function() x   ## function that returns the value of the matrix
  setinverse <- function(i) inverse <<- i  ## function that sets the (cached) inverse
  getinverse <- function() inverse  ## function that returns the (cached) inverse
  list(set = set, get = get,  ## return the list of functions
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the inverse of the matrix x. The inverse is only calculated
## if a value for the inverse is not already cached; otherwise the cached
## inverse is returned

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  ## gets the cached inverse
  if(!is.null(inverse)) {  ## if the inverse is cached then return it
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()  ## get the matrix
  inverse <- solve(data)  ## solve for the inverse
  x$setinverse(inverse)  ## set the cached value of the inverse
  inverse  ## return the inverse
  
}
