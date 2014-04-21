## These functions are built to cache the inverse of a matrix
## that could be high time-cosuming computation

## This function creates a special matrix which is really a list 
## containing the functions needed to cache the matrix 

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)

}


## This function calculates and returns the inverse of the special matrix

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    if(!is.null(i)){
        message("getting cache data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data , ...)
    x$setinverse(i)
    i
    
}
