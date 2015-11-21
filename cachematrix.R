## These two functions create a special matrix that
## contains various functions, then checks the matrix
## to see if the inverse has been calculated already, 
## and if it hasn't, calculates the inverse.

## This function creates a special matrix that 
## contains a list of functions that can set and
## get the value of the matrix and the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## This function checks to see whether the inverse
## of the special matrix created above has already
## been calculated, and if it hasn't, it calculates
## it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
