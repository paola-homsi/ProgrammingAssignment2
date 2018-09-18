## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
