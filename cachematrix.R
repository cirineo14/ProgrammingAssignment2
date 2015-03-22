## These two functions are used to create an object in the form of a list that
## stores a matrix and caches its inverse.

## This function creates the matrix-like object whose inverse can be cached.
## It returns a list of four functions that set the value of the matrix, get
## the value of the matrix, set the value of the inverse, and get the value of
## the inverse.

makeCacheMatrix <- function(X = matrix()) {
        I <- NULL       
        set <- function(Y) {
                X <<- Y
                I <<- NULL
        }
        get <- function() X
        setInv <- function(inverse) I <<- inverse
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function returns the inverse of the matrix-like object created with the
## makeCacheMatrix function. The inverse is either calculated, or retrieved from
## the cache.

cacheSolve <- function(X, ...) {
        I <- X$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- X$get()
        I <- solve(data, ...)
        X$setInv(I)
        I
}