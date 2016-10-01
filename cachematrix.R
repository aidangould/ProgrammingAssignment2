## These functions will store a matrix in an object, then solve it. If the matrix has been solved already, will be retrieved from the cache instead of being solved again, saving time.

## b is an solve-able matrix useful for testing whether the functions work as intended. Though it is commented out, it can be loaded by selecting the line (omitting the ##) and pressing Run.

## b <- matrix(c(4, 2, 7, 6), 2, 2)

## This function creates an object of the type makeCacheMatrix, which contains a list of four named elements. Those elements are functions which allow cacheSolve to work.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve is a function which computes the inverse of a matrix IFF the matrix has been loaded into an object of type MakeCacheMatrix, and stores the value in the cache; if cacheSolve has been run before, it retrieves the value from the cache instead of re-computing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
