## These functions avoid that the Inverse of a matrix has to be computed several
## times if the Inverse of the matrix was already computed once. 

## The function 'makeCacheMatrix' creates a "special Matrix" which is really a list containing
## a function to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the Inverse
## 4. get the value of the Inverse 

makeCacheMatrix <- function(x = matrix()) {

    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) I <<- Inverse
    getInverse <- function() I
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the Inverse of the "special Matrix" created with 
## the above function. However, it first checks to see if the Inverse has already been 
## calculated. If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the data and sets the value of the Inverse
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    I <- x$getInverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}
