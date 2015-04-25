## Functions that calculate the inverse of a matrix in the case that the inverse has not already been calculated. Inverse value is accessible through getMatrix() and setMatrix()
# method returned from the makeCacheMatrix method. 
##

## Function returns a list of functions that return and set a matrix and return and set the value of inverse. 

makeCacheMatrix <- function(matrix = matrix()) {
	inverse <- NULL
    setMatrix <- function(newMatrix) {
      matrix <<- newMatrix
      inverse <<- NULL
    }
    getMatrix <- function() matrix
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


##Function returns cached value for inverse (result of solve function). If is null, returns calculation of inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- matrix$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- matrix$getMatrix()
    inverse <- solve(data, ...)
    matrix$setInverse(inverse)
    inverse
}
