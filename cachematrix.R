## These are helpers functions to store the cached value
## of inverse matrix together with matrix itself


## This function creates new object (list) with member functions for getting/setting
## matrix value and inverse matrix value. The initial value for inverse matrix is empty

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the value of inverse matrix is defined already,
## calculates it if the value is not defined yet, and returns the value

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- m$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setinv(inv)
    inv
}
