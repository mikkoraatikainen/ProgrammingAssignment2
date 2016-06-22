## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
	## create variable inv and initialize with NULL value
    i <- NULL 
	## set the value of the vector
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
	## get the value of the vector
    get <- function() x
    ## set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    ## get the value of the inverse
    getinverse <- function() i
	## finally create list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
 }

## Test results:
## > B = matrix(c(1,2,3,4),nrow=2, ncol=2)
## > cacheSolve(makeCacheMatrix(B))
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
