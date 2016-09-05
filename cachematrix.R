## makeCacheMatrix and cacheSolve are functions designed to create a matrix
## object and compute its inverse using solve() function or return one from the
## cache, if it has already been calculated.

## makeCacheMatrix is a list of functions allowing to set a matrix and
## optionally its inverse and cache them.

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        setmatrix <- function(x) {
                m <<- x
                inv <<- NULL
        }
        getmatrix <- function() m
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve is a function designed to calculate the inverse of the matrix m
## created with makeCacheMatrix or return cached inverse, if it has already
## been computed or set via setinverse().

cacheSolve <- function(m, ...) {
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$getmatrix()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}
