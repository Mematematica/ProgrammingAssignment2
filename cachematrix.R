## In order to avoid solving a matrix many times we use this 2 functions:
## makeCacheMatrix takes a matrix and returns a list of 4 functions
## to be used by cacheSolve that takes them and returns the matrix inverse if
## its stored, if not then solves it, returns it and stores it.

# makeCacheMatrix creates the object used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#cacheSolve gets the inverse from cache or solves it if not cached.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Thank you for grading my code