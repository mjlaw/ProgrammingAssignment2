## Create a pair of functions that cache the inverse of a matrix similar to example
## makeVector() and cachemean()
## R Programming for Data Science (Coursera, JHU) Assignment2
## mjrobinette@gmail.com

## Creates an R object that stores a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) { # initialize x as an empty matrix 
    n <- NULL # initialize inverse of matrix to null
    
    # build a set of functions and return those functions in a list
    set <- function(y) { # assign y to x object in parent environment
        x <<- y
        # assign NULL to the n object in the parent environment and clear any
        # value of n that was cached by a prior execution of cacheSolve().
        n <<- NULL
    }
    
    get <- function() x  # retrieve x from the parent environment of makeCacheMatrix()
    
    # solve computes the inverse of a square matrix
    setinverse <- function(solve) n <<- solve #assign the input to value of n in parent environment
    
    getinverse <- function() n # retrieve n
    
    # assign each of the functions as an element within a list so functions can be accessed by name
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix. If the inverse has
## been calculated and the matrix has not changed, then cacheSolve retrieves the inverse 
## from cache. If the inverse has not been calculated, then compute the inverse through
## solve(x) and return its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse() # try to retrieve the inverse of matrix object
    if(!is.null(n)) { # if the inverse is not null
        message("getting cached matrix") # return a message 
        return(n) # return the inverse
    }
    
    # if n is null
    data <- x$get() # get the matrix from the input object
    n <- solve(data, ...) # compute the inverse
    x$setinverse(n) # set the inverse in the input object
    n
    # print the inverse object
}
