## This file contains functions that cache the result of solving a matrix (i.e.
## calculating its inverse).
## Usage:
##     m <- matrix(1:4, c(2,2)) ## could be any solvable matrix
##     cm <- makeCacheMatrix()  ## create cachedMatrix
##     cacheSolve(cm)           ## calculate and cache inverse
##     cacheSolve(cm)           ## use cached inverse

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
    cachedInverse <- NULL
    ## Setting matrix
    set <- function(m) {
        matrix <<- m
        cachedInverse <<- NULL
    }
    ## Returning matrix
    get <- function() matrix
    ## Caching calculated inverse
    setInverse <- function(inverse) cachedInverse <<- inverse
    ## Returning cached inverse, returns NULL if no inverse cached yet
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
    ## Return cached result if it exists 
    inverse <- cacheMatrix$getInverse()
    if(!is.null(inverse)) {
        message("returning cached inverse")
        return(inverse)
    }
    ## No cached result, calculate, store in cache and return result
    message("calculating and caching inverse")
    inverse <- solve(cacheMatrix$get(), ...)
    cacheMatrix$setInverse(inverse)
    inverse
}
