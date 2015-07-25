makeCacheMatrix <- function(x = matrix()) {
  ## introducing empty variable for the inverse of the matrix:
  inv <- NULL
  
  ## function to set the matrix: 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## functions to get the matrix and cache its inverse: 
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## If the inverse already exists (the inv in the line above isn't NULL), get it from cache:
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If it doesn't, solve the matrix to find it:
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}