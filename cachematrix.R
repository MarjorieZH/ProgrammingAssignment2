## Caching the Inverse of a Matrix
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set is direct y matrix to x while 'set' was called
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get is retrieve of matrix pre-defined
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  ## get_inverse is a storage of inversed matrix
  get_inverse <- function() m
  ##output of this function
  list(set = set, get = get, set_inverse = set_inverse
       , get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix()) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("Data has been calculated, retrieve the inverse from the cache")
    return(inv)
  }
  matri <- x$get()
  inv <- solve(matri)
  x$set_inverse(inv)
  inv
}
