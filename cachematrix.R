## This file contains functions that cache the result of solving a matrix (i.e.
## calculating its inverse).
## Usage:
##     m <- matrix(1:4, c(2,2)) ## any solvable matrix
##     cm <- makeCacheMatrix()  ## create cachedMatrix
##     cacheSolve(cx)           ## calculate and cache inverse
##     cacheSolve(cx)           ## use cached inverse

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
    cachedInverse <- NULL
    set <- function(m) {
        matrix <<- m
        cachedInverse <<- NULL
    }
    get <- function() matrix
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve retrieves the inverse from
## the cache.
cacheSolve <- function(cacheMatrix, ...) {
    inverse <- cacheMatrix$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(cacheMatrix$get(), ...)
    cacheMatrix$setInverse(inverse)
    inverse
}
