## These functions are able to cache potentially time-consuming computations 
## ie cache the inverse of a matrix.
## Let's assume that the matrix supplied is always invertible.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}


## to test the code:
## Run it
## Then: 
## create a matrix
##  > x <- matrix(1:4, 2, 2)
## create the special matrix which can cache its inverse
##  > m <- makeCacheMatrix(x)
## check the created matrix
##  > m$get()
##  [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
## compute the inverse of the matrix
##  > cacheSolve(m)
## you will see the result
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
## compute the inverse of the original matrix
## the difference is that the inverse has been already calculated
##  > cacheSolve(m)
## you will see a message
##  getting cached data
## and you also will see the same result
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5