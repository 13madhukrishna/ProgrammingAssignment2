## makeCacheMatrix() makes a special matrix. This function contains another list of functions
## to define the matrix, get the matrix, calculate the inverse of the matrix and 
## get the inverse of the matrix.

## solve() is the function used to find the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve)i <<- solve
  getsolve <- function()i
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inverse to the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)){
    message("Cached Data")
    return(i)  ## Return a matrix that is the inverse of 'x'
  }
  mtrx <- x$get()
  i <- solve(mtrx, ...)
  x$setsolve(i)
  i
  
}