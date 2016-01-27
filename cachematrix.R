## This function for programming assignment 2 for Coursera
## creates a matrix that can cache its inverse, 


makeCacheMatrix <- function(x = matrix()) {

    inv = NULL
    set = function (y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function computes the inverse of the matrix returned. If it has been calculated alread, 
## it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        
        x$setInverse(inv)
        
        return(inv)
}
