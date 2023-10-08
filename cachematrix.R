## an R program that implements the requested pair of functions using the <<- operator for caching the inverse of a matrix

## a function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
  # Initialize a variable to store the matrix
  matrixData <- x
  
  # Initialize a variable to store the cached inverse
  inversData <- NULL
  
  # Define a function to set the matrix
  set <- function(newValue) {
    matrixData <<- newValue
    inversData <<- NULL  # Reset the cached inverse
  }

  # Define a function to get the matrix
  get <- function() {
    matrixData
  }
  
  # Define a function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inversData <<- inverse
  }
  
  # Define a function to get the inverse of the matrix
  getInverse <- function() {
    inversData
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## a function to compute the inverse of the special "matrix" object and cache the result
cacheSolve <- function(matObj) {
  # Check if the inverse is already cached
  inverse <- matObj$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # If the inverse is not cached, compute it using solve()
  data <- matObj$get()
  inverse <- solve(data)
  
  # Cache the inverse
  matObj$setInverse(inverse)
  
  # Return the inverse
  inverse
}
