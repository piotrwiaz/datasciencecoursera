## Function makeCacheMatrix(x) returns list of functions that can :
##     - set the value of a matrix given as argument
##     - get the value of a matrix stored in cache memory
##     - set inverse of a matrix
##     - get inverse of a matrix stored in cache memory

## The function takes matrix as an input, and returns list of functions set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## Function cacheSolve computes inverse of a matrix obtained as an output of the 'makeCacheMatrix' function
## if the inverse has been already calculated, the function returns already calculated value, else it calculates it from scratch

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
