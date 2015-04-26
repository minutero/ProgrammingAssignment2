## This two functions working together will store a matrix's inverse
## so if tha calculation needs to happen again it will no be calculated 
## one more time, instead it will get the inverse from the cache version.

## This function creates a set of other functions in its environment
## for setting and then getting the initial matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a matrix, it can be calculated 
## or obtained from a previous calculation.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
