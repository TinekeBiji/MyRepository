
## These functions cache the inverse of the matrices so without any changes the inverse would not have to be
## calculated again, but loaded from cache. 

## This function creates a matrix which is really a list containing functions to operate the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function computes the inverse of the matrix created above, but firstly checks if the answer is not already
## available in cache memory. If so, the computation is skipped and the answer is loaded from cache. If not, the inverse
## is calculated and set in cache. 

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
