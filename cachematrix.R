##  This file calculate matrix inverse using caching

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
