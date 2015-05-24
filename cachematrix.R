## File cachematrix.R defines 2 functions:
## 1) makeCacheMatrix
## 2) cacheSolve
## 
## The functions are intended to work together to cache the inverse of a matrix
## (supplied by the user) in the global environment.  The purpose of caching is
## to speed up access to the inverse when the inverse is requested multiple
## times (such as in a for loop).
## 
## Usage:
## test1_special <- makeCacheMatrix(test1)
## test1_inverse <- cacheSolve(test1_special)
##
## where test1 is an ordinary matrix supplied by the user.
##

## The function makeCacheMatrix creates a special "matrix", which is really a
## list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # The inverse of the matrix.  Set to NULL initially.

        # Set the value of the matrix.  This function also sets the inverse of
        # the matrix to NULL.  The inverse of the matrix is set to NULL because
        # the matrix that is set does not have its inverse specified yet.
        #
        # This function uses the operator "<<-" to set the value of the matrix.
        # The "<<-" operator sets (i.e. caches) the value of the matrix in the
        # global environment.  Use of this operator makes subsequent retrievals
        # of the matrix faster (since its value is cached in the global
        # environment).
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        # Get the value of the matrix.
        get <- function() {
                x
        }

        # Set the inverse of the matrix.
        #
        # This function uses the operator "<<-" to set the inverse of the matrix.
        # The "<<-" operator sets (i.e. caches) the inverse of the matrix in the
        # global environment.  Use of this operator makes subsequent retrievals
        # of the inverse faster since its value is cached in the global
        # environment.
        setinverse <- function(inv) {
                m <<- inv
        }

        # Get the inverse of the matrix.
        getinverse <- function() {
                m
        }

        ## Return a list containing functions to set and get the value of the
        ## matrix, and set and get the value of the inverse of the matrix.
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## The function cacheSolve calculates the inverse of the special "matrix"
## created by the function "makeCacheMatrix". However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setinverse
## function (the setinverse function of the special "matrix").
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Check to see if the inverse exists (i.e. it is not NULL).  If so,
        ## return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the inverse does not exist, do the following:
        ## 1) get the matrix
        ## 2) compute the inverse matrix
        ## 3) set the inverse matrix
        ## 4) finally return the inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
