## Function makeCacheMatrix returns a list of functions 
## set - stores the matrix passed in
## get - returns the matrix
## setinv - computes and caches the inverse of the matrix
## getinv - retrieves the inversed matrix from cache


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


## Function cacheSolve computes the inverse of a matrix
## if the cached value is found, return it.
## else compute the inverse and store it using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
