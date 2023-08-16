# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix
  set <- function(newValue) {
    x <<- newValue
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() {
    x
  }
  
  # Set the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of a matrix and cache it
cacheSolve <- function(mat, ...) {
  inv <- mat$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse and cache it
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setInverse(inv)
  
  inv
}
