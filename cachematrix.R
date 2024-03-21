## Function: makeCacheMatrix

## A list containing methods to manipulate and retrieve the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {        
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}

##Fuction: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the
## function retrieves the inverse from the cache.= Return a matrix that is the inverse of "x"

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
