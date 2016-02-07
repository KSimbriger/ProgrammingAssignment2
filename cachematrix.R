## The following to functions create a special object to speed up the
## computation of inverses of matrices, by cahing the solutions.

## This function creates a "matrix", that is rather a list containing a function
## that:
## * sets the value of the matrix
## * gets the value of the matrix
## * sets the value of the inverse of the matrix
## * gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the "matrix" created with the function
## above. It checks first, if the inverse has already been calculated. If so,
## the function skips the computation step and and gets the cached data. Else, 
## it caculates the inverse of the matrix fed into the function and stores it in
## the cache, using the setinverse() function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
