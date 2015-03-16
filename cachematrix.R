## Coursera R Programming, Assignment 2 (Prprog-012)
## Functions used to calculate, cache and retrieve the inverse of a given (Invertible) matrix.
## Important: The input matrix is assumed to always be Invertible.


## Returns a CacheMatrix object.
## This helper object is not an actual matrix but  a list containing functions that:
##    set the value of the matrix
##    get the value of the matrix
##    set the value Inverse of the input matrix (careful, not "compute")
##    get the value Inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Returns a matrix that is the inverse of 'x'
## If the inverse has already been computed, it retrieves the cached result without recomputing it.
## If not, it the inverse matrix computed and stored in the cache before being returned.
## Params:
##  x: a CacheMatrix object, generated using the makeCacheMatrix function defined above. The function doesn't work
##     given a native R matrix as an input parameter.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
