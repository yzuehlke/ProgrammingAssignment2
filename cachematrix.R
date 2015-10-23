## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # set value of the matrix
    setMatrix <- function(newValue) {
        x <<- newValue
        m <<- NULL
    }
    
    # returns the value of the matrix
    getMatrix <- function() x
    
    # cache the inverse 
    cacheInverse <- function(solve) m <<- solve
    
    
    # get the cached inverse
    getInverse <- function() m
    
    # return a named list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the cached inverse
    im <- x$getInverse()
    
    # if a cached inverse exists --> return it
    if(!is.null(im)) return(im)
    
    # if a cached inverse does not exist --> calculate the inverse and cache it
    data <- x$getMatrix()
    
    im <- solve(data)
    
    x$cacheInverse(im)
    
    # return the inverse
    im
}
