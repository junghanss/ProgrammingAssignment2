## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## SetMethod to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## GetMethod the get the matrix
  get <- function() {
    x
  }
  
  ## SetMethod to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## GetMethod to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Now Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if( !is.null(inverse) ) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(inverse)
  
  inverse
}
