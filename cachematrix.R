## The following functions are used to Solve a square matrix and cache the 
## resultant data in variable m which is globally stored because of the <<- assignment operator

## The makeCacheMatrix creates a vector of functions which are used to get and set the matrix values
## and to assign and retrieve the inverse.  These functions are called in turn by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function is run using the output of makeCacheMatrix.
## This function first assigns the getinverse() function value from makeCacheMatrix
## If the output of getInverse is null, the function calculates the inverse using solve() 
## and assigns that value to m using the setInverse() function from makeCacheMatrix.

## Because makeCacheMatrix uses the <<- assignment operator, m is assigned in cacheSolve.
## when cacheSolve is next run, it executes getInverse() which attempts to assigm m.  
## Since no m is input to the function, it searches for the value of m in cacheSolve, 
## which is the output m from the first run.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
