## First function creates a list of functions that:
##  1. sets the value of the matrix
##  2. gets the value of the matrix
##  3. sets the inverse of the matrix
##  4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks to see if the inverse of the matrix has already been
## calculated, and if not, calculates it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
