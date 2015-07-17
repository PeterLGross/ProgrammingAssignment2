## These functions create a special object that stores a numeric matrix
## and caches its inverse matrix. It is assumed that the matrix is
## a square invertible matrix.

## This function creates a matrix as a list of functions to...
## 1) Set the value of the matrix (i.e. change it to something new)
## 2) Get the current value of the matrix
## 3) Set the value of the inverse of the matrix
## 4) Get the current value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULLmat
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes an an argument a matrix stored as a list
## by the makeCacheMatrix function. It first checks to see if the
## inverse matrix has already been caculated, and if so, retrieves
## the previously calculated inverse matrix. If the inverse matrix
## has not previously been calculated, the function calculates the
## inverse and caches the result. Either way, the function returns
## the value of the inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
