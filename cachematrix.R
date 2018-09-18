## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : function that cache the inverse of an x matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve : function that get the inverse of a matrix if cached or compute the inverse and 
##              cache it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get()
    if (!is.null(x$getInverse())) {
        message('getting data')
        return(x$getInverse())
    }
    data <- x$get()
    inv <- solve(data)
    message('setting the inverse')
    x$setInverse(inv)
    x
}
