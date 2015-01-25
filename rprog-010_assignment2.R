##----------------------------------------------------------##
##      R Programmining Assignment 2                        ##
##----------------------------------------------------------##


##----------------------------------------------------------##
##      makeCacheMatrix Function                            ##
##----------------------------------------------------------##
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) m <<- inverse
    get_inv <- function() m
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


##----------------------------------------------------------##
##      makeCacheMatrix Function                            ##
##----------------------------------------------------------##

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data.")   
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$set_inv(m)
    m
}

