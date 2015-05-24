## cachematrix.R
## 23 May 2015
## Programming Assignment 02

## These functions are based on the examples makeVector and cachemean
## provided in the Programming Assignment 2 instructions.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrixinverse <<- solve
        getinverse <- function() matrixinverse
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
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}