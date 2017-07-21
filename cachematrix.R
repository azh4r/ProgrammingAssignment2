## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes an inversible matrix and returns a list of functions
## that set and get the value of the inversible matrix and 
## set and get values of the inverse of the matrix

## Creates a cached Matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## initializes the value of the inverse to NULL
    inv <- NULL
    ## setter function for the input (reversible) matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    
    ## getter for the input (reverible matrix)
    get <- function() x
    
    ## setter for inverse matrix, this is where the inverse is cached
    setinv <- function(inverse) inv <<- inverse
    
    ## getter for the cached inverse matrix
    getinv <- function() inv
    
    ## return a list object that contains the above functions.
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)

}


## calculates the Inverse of a Matrix. If inverse is already calculated then it just retrieves it from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get the value of the cached inverse from the "special" matrix
    inv <- x$getinv()
    
    ## if the value is not null then inverse has already been calculated so return it
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    ## value was null so calculate the inverse using solve()
    data <- x$get()
    inv <- solve(data)
    
    ## cache the calculated inverse in the special matrix and return it. 
    x$setinv(inv)
    inv
}
