## Put comments here that give an overall description of what your
## functions do
##  This file calculate matrix inverse using caching

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Take a matrix that can be inversed, and caching the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
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


## Write a short comment describing this function
## Compute the inverse of matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                print("getting cached matrix")
                return(inv)
        }
         matrix <- x$get()
         inv <- solve(matrix, ...)
         x$setinv(inv)
         return(inv)
         
}
