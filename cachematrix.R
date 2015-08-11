## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix stores a list of four functions; get(), set(), getinv() and 
# setinv(). It is used only to store or alter the values but not to 
# calculate.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set() changes the values of argument x of makeCacheMatrix()
        set <- function(y){     # Takes y(matrix) as input
                x <<- y         # Assigns y to x in parent frame
                inv <<- NULL    # Since matrix is changed, inverse must be
                                # reassigned to NULL
        }
        get <- function() x     		# returns matrix x
        setinv <- function(sinv) inv <<- sinv   # Sets input as inv in parent
                                                # frame
        getinv <- function() inv        	# gets inv
        list(set=set, get=get, setinv = setinv, getinv=getinv) # List of 4 fs
}


## Write a short comment describing this function
# cacheSolve takes makeCacheMatrix object as input. 
# It first checks if the inverse of matrix exists in getinv() of 
# makeCacheMatrix() and returns it if available. If not available then 
# calculates it using solve() and returns the answer.

cacheSolve <- function(x, ...) { # Takes makeCacheMatrix() object as input
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()       # Gets getinv() from makeCacheMatrix()
        if(!is.null(inv)) {     # Checks if inv has stored value
                message("getting cached data") 
                return(inv)     # returns inv
        }
        data <- x$get()         # if inv is not stored, get the matrix to be
                                # inverted and store in 'data'
        inv <- solve(data, ...) # Invert the matrix 'data'
        x$setinv(inv)           # store the inverse in makeCacheMatrix() 
                                # using setinv()
        inv
}
