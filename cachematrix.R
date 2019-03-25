## makeCacheMatrix creates a list of functions
## the following 4 functions are in the list and serve as input for cacheSolve
##        1. set the matrix
##        2. get the matrix
##        3. set the inverse matrix
##        4. get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## returns cached matrix if it can be found
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    ## returns computed matrix if no cached matrix is found
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

