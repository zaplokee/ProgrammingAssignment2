## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## For this assignment, assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL        #clear cached
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)    #returns a list
}


## cacheSolve computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
