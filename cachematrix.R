## This code investigates lexical scoping and perserved variables that are used with lengthly computations.
## The first function sets up the cached variables and objects used to control the processing of subsequent functions.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                # initialize cached variables 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        # parameters used to set function logic of getting exisiting matrix or inverse of matrix  
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   
        # check to see if cached data (inv) exists, returns inv if present
        if(!is.null(inv)) {
                # return existing cached data
                return(inv)
        }
        # cached data does not exist; compute inverse of matrix with getinverse function in makeCacheMatrix
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        # return inverse of matrix
        inv
}