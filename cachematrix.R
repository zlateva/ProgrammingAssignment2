## makeCacheMatrix takes as argument a matrix x
## returns list of functions to set and get the matrix x and its inverse inv
## the set and setinverse use the super-assignment operator '<<-' for 
## setting values for x and inv in the local environment, 
## thus providing a state for list objects created with makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL             ##inv initialized to NULL indicates must be computed
            set <- function(y) {
                    x <<- y
                    inv <<- NULL  ##inv set to NULL after new value assigned indicates must be computed
            }
            get <- function() x
            setinverse <- function(solve) inv <<- solve
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## cacheSolve takes as argument list object x created with makeCacheMatrix
## returns inv, the inverse of matrix in list object x
## it computes the inverse only if the local value inv of the inverse of x is not NULL
## otherwise it returns the cached value of inv 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
            inv <- x$getinverse()
            if(!is.null(inv)) {   ## checks if inverse matrix is already computed
                    message("getting cached data")
                    return(inv)   ## returns cached value if inv != NULL
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv

}
