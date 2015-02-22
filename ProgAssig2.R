## Set of functions to cache inverse matrix. Used to reduce computation while resolving algebra tasks.
## NOTE: This function do not check if the given matrix is inversable.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean
## Function makeCacheMatrix is more like a constructor for object with a set of functions to be triggered on it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
## As a constructor makeCacheMatrix allows it, the function cacheSolve first checks if the inversion has not been allready made on object.

cacheSolve <- function(x, ...) {
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