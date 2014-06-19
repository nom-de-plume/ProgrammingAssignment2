## These functions are analogs of the vector/mean caching 
## functionaility in the preface of the assignment
## makeCacheMatrix stores the matrix and inverse; and
## cacheSolve does the actual inverse

## makeCacheMatrix - this function stores the matrix and inverse
## it takes as input x (matrix) 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve - this function calculates the inverse of a matrix x 
##  (created from makeCacheMatrix function)
##   it calculates the inverse and stores the inverse if it is not
##   calculated yet
##   returns the inverse
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv		
}
