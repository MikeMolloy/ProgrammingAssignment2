## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function takes a matrix argument and returns a "matrix
## object", actually a list.  the list has 4 members, each a function.
## the function set takes an argument and assigns it to an internal
## variable, matrix, and sets inv (an internal variable of
## makeCacheMatrix) to NULL.  the function get takes no arguments, and
## returns matrix, the parameter to makeCacheMatrix.  the function
## setinverse takes an argument, inverse, and assigns it to
## makeCacheMatrix's variable, inv.  the function getinverse takes no
## arguments and returns the object, inv.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function takes a "matrix object" argument (actually a list of
## the sort returned by makeCacheMatrix), checks to see whether that
## object has as member the cached inverse of a matrix and, if so,
## cacheSolve returns that inverse without doing anything more.  if
## the matrix argument does not have a cached inverse, then cacheSolve
## uses the R function, solve, to find the inverse of the matrix
## argument to cacheSolve, caches that inverse in the "matrix object",
## and returns the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    invertibleMatrix <- x$get()
    inv <- solve(invertibleMatrix, ...)
    x$setinverse(inv)
    inv
}
