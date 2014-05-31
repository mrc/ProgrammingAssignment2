## cachematrix.R
##
## Computing the inverse of a matrix can be a time consuming
## process. If the matrix does not change, its inverse does
## not change, so it is a reasonable strategy to store the
## inverse when it is first needed, and retrieve the stored
## value if it is needed again, which avoids the need to
## recompute it.

## makeCacheMatrix:
## Create a list of functions which can be used to store and
## manipulate a square invertible matrix and its inverse.
##
## If the matrix is altered (with the "set" function), any
## previously cached value is likely to be invalid, so the
## cache is reset.
##
## Returns this list of functions:
##   set: set the value of the matrix
##   get: get the value of the matrix
##   setinverse: set the cached value of the inverse
##   getinverse: get the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list (set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve:
## Return a matrix which is the inverse of the matrix created by
## the "makeCacheMatrix" function.
##
## The first time this function is run for a matrix, it
## computes the inverse and stores it, and on subsequent runs it
## will return this stored value, avoiding the need to compute it
## more than once.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
