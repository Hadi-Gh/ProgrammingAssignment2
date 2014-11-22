## Put comments here that give an overall description of what your
## functions do


## This finction creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
         getInverse = getInverse)
}

## THis function computes the inverse of the special "matrix" returned by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting Cached Matrix Inverse")
        return(m)
    }
    y <- x$getMatrix()
    m <- solve(y, ...)
    x$setInverse(m)
    m
}
