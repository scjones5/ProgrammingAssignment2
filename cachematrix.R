## Functions to invert a matrix. The inverse is first cached
## such that repeat computations are not necessary. 

## Accepts a matrix and then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv, 
         getInv = getInv)
}


## Accepts the result of makeCacheMatrix, either computing an inverse, 
## or retrieving the cached result

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
