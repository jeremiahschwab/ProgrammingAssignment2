## These functions cache the inverse of a matrix
## so it will not need to be calculated again


## This function creates an object that has four functions, set the matrix, 
## return the matrix, set its inverse, and return its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function checks if the inverse of this particular matrix has already been calculated
## If so, it returns the cached answer. If not, it calculates the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}