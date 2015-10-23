## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
    
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Write a short comment describing this function

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
