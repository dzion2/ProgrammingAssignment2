# Caching matrix inverse.


#' Creates an object that caches inverse of a given matrix.
#' @param x matrix which inverse should be cached.
#' @return list with functions to set matrix, get matrix,
#'         set matrix inverse, get matrix inverse.
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

#' Gives the inverse from the cache object.
#' Caches the inverse if it hasn't been cached yet.
#' @param x cached object
#' @return inverse of the matrix.
#'
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
