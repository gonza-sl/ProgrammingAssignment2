## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly.


## Encapsulates a matrix and an it's inverse calculated using "cacheSolve"
## It's used to cache the solution to the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes a matrix created with "makeCacheMatrix" and return it's inverse.
## If it's the 1st time, it calculates the inverse using solve(). Otherwise
## it return the cached solution.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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


# Test
#set.seed(1)
#ma <- makeCacheMatrix(matrix(rnorm(25),5,5))
# Print solution
#cacheSolve(ma)
# Test solution: matrix multiplication
# right answer is ones in the diagonal, zero or close for the rest
#ma$get() %*% cacheSolve(ma)
