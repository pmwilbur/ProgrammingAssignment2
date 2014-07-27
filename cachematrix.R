
## The functions makeCacheMatrix and cacheSolve create a cache of inverted matrices.  When cacheSolve
## is called with a matrix, the functions check to see if the inverted matrix is in the cache, and
## if so, return it along with a message stating that the inverted matrix was pulled from the cache.
## If the matrix is not in the cache, the functions invert the matrix and place
## it in the cache before returning the inverted matrix.


## Function makeCacheMatrix takes matrix x and inverts it.  The function maintains a cache
## that holds previous matrix inverts.  If the same matrix invertion is requested, the function
## will return the value in the cache.

makeCacheMatrix <- function(xm = matrix()) {
    im <- NULL
    set <- function(y) {
        xm <<- y
        im <<- NULL
    }
    get <- function() xm
    setinvert <- function(solve) im <<- solve(xm)
    getinvert <- function() solve(xm)
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)  
}



## cacheSolve takes a matrix and calls the functions from makeCacheMatrix to
## either invert the matrix and put it in the cache or returns an inverted matrix
## from the cache.

cacheSolve <- function(xm, ...) {
    x <- makeCacheMatrix(xm)
    im <- x$getinvert()
    if(!is.null(im)) {
        message("getting cached matrix inversion")
        return(im)
    }
    data <- x$get()
    im <- solve(data,...)
    x$setinvert(im)
    im
}

