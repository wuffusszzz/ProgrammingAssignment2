## Put comments here that give an overall description of what your
## functions do

##There are two functions: makeCacheMatrix and library(MASS)
##makeCacheMatrix consists of set, get, setinverse, and getinverse
library(MASS) #calculates the inverse for non squared as well as squared matrices

makeCacheMatrix <- function(a = matrix()) {
    inv <- NULL                    ##initializes inverse as NULL
    set <- function(b) {
        a <<- b
        inv <<- NULL
    }
    get <- function() a          ##function to get matrix a
    setinv <- function(inv) inv <<- inverse
    getinv <- function () {
        inv2 <- ginv(a)             ##function to obtain inverse of the matrix
         a%*%inv2%               ##check that a*inv(a) = identity matrix, note products here are cross products not dot products
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(a, ...)    ##gets cache data
    {
        inv <- a$getinv()
        if(!is.null(inv)) {       ##checking whether inverse is NULL
            message("getting cached data!")
            return(inv)           ##returns inverse value
        }
        data <- a$get()
        inv <- solve(data, ...)   ##calculates inverse value
        a$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'a'
}
