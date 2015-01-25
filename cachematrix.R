## Here are two functions to create a special object
## that stores a matrix and cache's its inverse matrix.
## The functions assume that the matrix supplied is always invertible

## This function creates a special "matrix", which is really a list
## containing a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the inverse matrix
##   - get the inverse matrix
## A special "matrix" object created by this function can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}