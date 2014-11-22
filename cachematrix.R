## The inverse of a matrix is calculated and stored as a list
## in a separate memory environment.
## Subsequent calls with the same list as input will use the 
## cached value instead of doing the calculation

## Calculate and store the inverse of a matrix to a list
## Input : a square matrix
## Output: a list with set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
    # Set the list with matrix and NULL inverse
    m <- NULL
    set <- function(y) {
        # set matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x 
    setinv <- function(solve) m <<- solve 
    getinv <- function() m
    #return list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculate inverse of matrix if new or recall if exists
## Input : list output from makeCacheMatrix
## Output: Inverse of matrix with message if cached matrix is used 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    # Does the inverse exists?
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Inverse does not exists, calculate it
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
