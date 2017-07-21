## Put comments here that give an overall description of what your
## functions do

## Creates a cached Matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)

}


## calculates the Inverse of a Matrix. If inverse is already calculated then it just retrieves it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
