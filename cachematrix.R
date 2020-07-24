

## This function creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        q <- NULL
        ##sets value of the vector
        set <- function(y) {
                x <<- y
                q <<- NULL
        }
        ##gets value of the vector
        get <- function() x
        ##sets the inverse matrix
        setinverse <- function(inverse) q <<- inverse
        ##gets the inverse matrix
        getinverse <- function() q
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" from above fn
##If calculated it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        q <- x$getinverse()
        ##checks if the inverse is retrieved in q or not
        if(!is.null(q)) {
                message("getting cached data")
                return(q)
        }
        ##solves for inverse of the matrix
        matr <- x$get()
        q <- solve(matr, ...)
        x$setinverse(q)
        q
}
