## makeCacheMatrix codes a set of functions and specifies a couple of variables
## within its parent environment.  That allows the next function to use cached results

makeCacheMatrix <- function(x = matrix()) {

        inv <- matrix(NA,nrow(x),ncol(x))
        set <- function(y) {
                x <<- y
                inv <<- matrix(NA,nrow(x),ncol(x))
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of the matrix specified in the former function
## unless that result has been calculated and stored as cached data before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(all(!is.na(inv))) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}