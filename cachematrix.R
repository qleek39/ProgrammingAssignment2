## Put comments here that give an overall description of what your
## functions do

## this function creates a closure (an environment) with two variables x (for the matrix data), and invX 
## (to store its inverse matrix of x), and 4 functions. It then return the list of 4 functions of the closure (so 
## the four functions can always access the two variables m and invM defined in it)

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    # set the variable x of the parent environment to the matrix data y
    x <<- y
    # reset the inverse matrix invX of the parent environment to NULL
    invX <<- NULL
  }
  
  get <- function () {return (x)}
  
  # function set the inverse matrix (the inverse matrix must be calculated before this setting) in the parent environment
  # of the function
  setInvMatrix <- function(invMatrix) {invX <<- invMatrix}
  # function to get the value of the inverse matrix
  getInvMatrix <- function() { return (invX)}
  
  # return the list of 4 functions created in the closure (the closure also includes 2 defined variables)
  return (list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix))

}


## this function uses the function getInvMatrix of x to get its inverse matrix (defined in the closure).
## if the inverse matrix of x is cached before it returns the value and avoid calculating the inverse matrix again
## if the inverse matrix is null (not calculated before), it uses the solve function to calculate the inverse matrix
## and cache the inverse matrix to the variable invX of the closure

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  
  if (! is.null(invMatrix)) {
    message("getting cached data")
    return (invMatrix)
  }
  # the inverse matrix is not calculated before, calculate it now
  # get the matrix data
  m <- x$get()
  # calculate the inverse matrix of m
  invMatrix <- solve(m, ...)
  # cache the inverse matrix 
  x$setInvMatrix(invMatrix)
  
  # return the inverse matrix of x
  return (invMatrix)
}
