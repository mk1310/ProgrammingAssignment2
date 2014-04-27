## These functions are designed to calculate the inverse of a matrix in an efficient way.
## Matrix inversion is usually a costly computation &
## these functions will help in caching the inverse of a matrix rather than compute it everytime.

## The first function, makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse of matrix
        setsolve <- function(solve) s <<- solve
        ## get the value of the inverse of matrix
        getsolve <- function() m
        ## create the list containing all the functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## assigns value s matrix, which is inverse of the matrix x
        s <- x$getsolve()
        ## check if s matrix is not null, if yes returns s matrix
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## if not, get the inverse of the matrix  
        data <- x$get()
        s <- solve(data, ...)
        ## and set the inverse of matrix
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
