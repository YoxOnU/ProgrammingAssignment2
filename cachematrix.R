## These functions create a matrix and a storage/cache for its inverse, and
## retrieve the cached value if it already has been computed and stored.

## "makeCacheMatrix" creates a matrix object that can store its inverse (cache)
## If the matrix "x" is altered the previous cached inverse is reset to NULL

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


## "cacheSolve" computes the inverse of the matrix object created earlier "x"
## If the inverse has been computed already, it will retrieve and return the
## cached value. If not it will compute the inverse using solve(), store it
## in the cache, and return it.

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
        ## Return a matrix that is the inverse of 'x'
}

