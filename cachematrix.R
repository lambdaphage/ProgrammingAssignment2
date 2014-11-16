## The following functions are meant to cache a value for the
## inverse of a matrix to decrease potential overhead.

## Create a matrix object that the inverse can be stored to.

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(y){
    x       <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(get = get, set = set,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of a matrix unless already cached.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("Retrieving cached value")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
