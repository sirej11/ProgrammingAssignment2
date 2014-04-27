## This functions create a specail "matrix" that can chache its inverse
## base on sample function makeVector and chacheVector

## Create special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set      = set, 
       get      = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculate matrix inverse, if stored skip calculation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

