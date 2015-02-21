## These functions cache the inverse of a matrix, using scoping rules of the R language. 
## This avoids the potentially time-consuming process of calculating the inverse of an
## invertible matrix if it has already been previously calculated. 

## The function makeCacheMatrix creates a special "matrix", which is a list containing 
## a function to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix" created with
## the function makeCacheMatrix. It first checks to see if the inverse has already
## been calculated. If so, it obtains the inverse from the cache and skips the
## computation; otherwise, it calculates the inverse and sets the inverse of the
## matrix in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
