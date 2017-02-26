##This assignment is modeled after the exmaple for caching meahs that was provided

## This function stores the matrix provided by the user and solves for the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) m <<- inverse
    getsolve <- function() m
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}


## This function resturns the inverse of the matrix from the above if already available and if not solves for the inverse

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## To run this function assign the makeCacheMatrix function to A and store its value in m. Call cacheSolve(m) to resolve for the inverse.
## A <- makeCaheMatrix(matrix(1:4, 2, 2))
## m <- A
## cacheSolve(m)
## The solve function looks for arguments solve(a, b, ...). 'a' can be a matrix in this case
## and in the absence of 'b', b is taken to be an indentity matrix and solve returns inverse of a

