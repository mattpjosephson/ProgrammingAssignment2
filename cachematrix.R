
## function will create a matrix 'x' that will later be inverted
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL   ## i will eventually be inverse of matrix, but at this point is not calculated yet, so NULL
        set <- function(y) {    ## set is a function that changes the matrix stored originally to be y
                x <<- y         ## sets matrix x (<<- means from makeCacheMatrix, not within set function) to be  y (the input)
                i <<- NULL      ## sets i (<<- means from makeCache Matrix, not within set function) to be NULL, 
        }                      
        get <- function() x     ## get is a function that returns "x"
        setinverse <- function(solve) i <<- solve       ##set i (from original makeCacheMatrix) to 'solve'
        getinverse <- function() i ## returns the value i (lexical scoping finds i by searching all enviornments )
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve will calculate, or return a previously cached value for the inverse of matrix 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinverse()             ## searches getinverse function for cached value of inverse of 'x'
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)       ## returns cached value for inverse of 'x' if present
        }
        data <- x$get()                 ## if no cached value for inverse of 'x', then it is calculated, cached, and printed.
        i <- solve(data, ...)
        x$setinverse(i)
        print(i)
}