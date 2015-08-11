## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix stores a list of four functions; get(), set(), getinv() and 
# setinv(). It is used only to store or alter the values but not to 
# calculate.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(sinv) inv <<- sinv
        getinv <- function() inv
        list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function
# cacheSolve takes makeCacheMatrix object as input. 
# It first checks if the inverse of matrix exists in getinv() of 
# makeCacheMatrix() and returns it if available. If not available then 
# calculates it using solve() and returns the answer.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
