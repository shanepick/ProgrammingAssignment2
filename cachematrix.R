## Used for the calculation of the inverse of a matrix such that the inverse
## is only calculated once, and all subsequent calls to cacheSolve() will return
## the previous cached value, rather than re-calculating each time.


## Returns a list of functions to get/set the value of matrix x and 
## get/set the the inverse for a matrix x.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## returns the inverse of a matrix when passed the list of functions for
## a matrix as returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
