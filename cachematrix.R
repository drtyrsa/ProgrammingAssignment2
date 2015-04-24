## Functions for memoization of matrix inverse calculation
## Programming assessment for rprog-013 Coursera class 


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cached_value <- NULL
  set <- function(some_matrix) {
    x <<- some_matrix
    cached_value <<- NULL
  }
  get <- function() x
  setsolve <- function(solve_value) cached_value <<- solve_value
  getsolve <- function() cached_value
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  solve_value <- x$getsolve()
  if(!is.null(solve_value)) {
    message("getting cached data")
    return(solve_value)
  }
  original_matrix <- x$get()
  solve_value <- solve(original_matrix, ...)
  x$setsolve(solve_value)
  solve_value
}
