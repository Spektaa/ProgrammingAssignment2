## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. It includes methods to set and get
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  set <- function(y) {
    x <<- y     # Assign the new matrix to x
    inv <<- NULL  # Reset the cached inverse when a new matrix is set
  }
  
  get <- function() x  # Function to get the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
  
  getInverse <- function() inv  # Get the cached inverse
  
  # Return a list of the methods for interacting with the special "matrix"
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then it retrieves the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if it exists
  
  if (!is.null(inv)) {
    message("Getting cached data")  # Notify that cached data is being used
    return(inv)  # Return the cached inverse
  }
  
  mat <- x$get()  # Get the matrix from the special "matrix"
  inv <- solve(mat, ...)  # Compute the inverse of the matrix
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
