## Function create a special 'matrix' object 
## which provides get/set functions for original matrix
## and getsolve/setsolve functions for cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(new) {
        x <<- new
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(new) m <<- new
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function takes a special 'matrix' object and 
## returns a cached inverse matrix or create new one.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}

