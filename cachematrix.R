## cachematrix.R
## 23 May 2015
## Programming Assignment 02

## These functions are based on the examples makeVector and cachemean
## provided in the Programming Assignment 2 instructions.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the matrix inverse to null
        matrixinverse <- NULL
        ## Create a function to set the matrix values
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        ## Create a function to get the matrix values
        get <- function() x
        ## Create a function to solve for the matrix inverse
        setinverse <- function(solve) matrixinverse <<- solve
        ## Create a function to get the matrix inverse
        getinverse <- function() matrixinverse
        ## Return the four functions as a vector
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Examine the getinverse field
        matrixinverse <- x$getinverse()
        ## If getinverse is not null, the inverse has already
        ## been calculated, so return that value
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        ## Otherwise, get the matrix value
        data <- x$get()
        ## Compute the matrix inverse
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        ## Return the inverse value
        matrixinverse
}