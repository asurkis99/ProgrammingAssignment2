## These functions create a special matrix that can cache its inverse and
## then retrieves the inverse if it has been cached or calculates & caches 
## it has not yet been cached

## This function creates a list of functions for setting/getting value 
## of matrix and for setting/getting value of the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns a cached value of the matrix inverse if it exists
## if there is no cached value, it calculates the inverse and caches it
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
