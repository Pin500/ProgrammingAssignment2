## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL  # Initialize cache for inverse
  
  set <- function(y) {
    x <<- y      # Assign new value to the matrix
    inv <<- NULL # Reset cached inverse
  }
  
  get <- function() x                # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getInverse <- function() inv       # Retrieve cached inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get cached inverse
  
  if(!is.null(inv)) {    # Check if inverse is cached
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)        # Cache the inverse
  inv
        ## Return a matrix that is the inverse of 'x'
}
