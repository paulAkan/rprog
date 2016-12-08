## this function stores and set the matrix and its inverse
## it is an input to another function cacheSolve


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setm <- function(inverse) m <<- inverse
        getm <- function() m
        list(set=set, get = get, setm = setm, getm = getm)
}


## this function checks if the inverse exits
##if the inverse exits, it is recall from cache
##otherwise, it is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getm()
        if (!is.null(m)){
                message("getting cached data...")
                return(m)
        }
        ##find the inverse of the matrix
        data <-x$get()
        m <- solve(data, ...)
        x$setm(m)
        return(m)
}
