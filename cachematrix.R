## These functions cache the inverse of a matrix
## so it will not need to be calculated again


## This function creates a list that has four functions, store a matrix, 
## return the stored matrix, set its inverse, and return its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## Stores a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Returns the stored matrix
  get <- function() x
  
  ## Stores the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  ## Returns the inverse of the matrix
  getinverse <- function() m
  
  ## Creates the list of these functions
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function checks if the inverse of this particular matrix has already been calculated
## If so, it returns the cached answer. If not, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
  ## Assigns the results of the getinverse function to m
  m <- x$getinverse()
  
  ## Checks if m is null. If not, advises that it did not need to be calculated and returns the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Calculates the inverse of the matrix, caches it, and returns it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}