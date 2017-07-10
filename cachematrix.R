## A pair of functions that use lexical scoping to cache the inverse of a matrix.
## Assumes the input matrix is an invertible square matrix.

## Returns a list which contains functions to set/get the matrix and 
## set/get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Returns the inverse of an invertible square matrix. If the inverse has already been
## computed, it returns it from cache, otherwise it calculates the inverse using solve().

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    
}
