## The makeCacheMatrix function caches the inverse of a reversible square matrix
##The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function. If the inverse has already been calculated, CacheSolve retrieve the inverse from the cache.

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the matrix inverse
#4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <-function() x
        setinv <- function(inverse) inv <- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve calculates the inverse if a matrix created by the makeCacheMatrix function
## It checks to see if the invert has already been computed. If so it gets it  the from the cache.
## Else it computes the inverse, caches it and then returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
            message("retrieving cached data")
            return (inv)
        }
        data <- x$get()
        inv2 <- solve(data, ...)
        x$setinv(inv2)
        inv2
}
