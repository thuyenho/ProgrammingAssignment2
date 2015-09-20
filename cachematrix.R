## This function take a matrix A
## Return a special matrix that can be cached.
makeCacheMatrix <- function(A = matrix()) {
    
    # The first time, inverse of a Matrix was not computed
    # so that, set to NULL.
    inversedMatrix <- NULL
    
    
    set <- function(m) {
        A <<- m
        inversedMatrix <<- NULL
    }
    
    get <- function() A
    
    setInversedMatrix <- function(inversedMatrix) inversedMatrix <<- inversedMatrix
    getInversedMatrix <- function() inversedMatrix
    
    list(set = set, get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}



## This function returns the inverse of matrix of the special maxtrix A
## and is able reduce time-consuming computation. 
cacheSolve <- function(A, ...) {
    
    inversedMatrix <- A$getInversedMatrix()
    
    # The inverse of maxtrix of A has been compteted  before.
    # So that, returns inverse of matrix immediately.
    if(!is.null(inversedMatrix)) {
        message("getting cached data")
        return(inversedMatrix)
    }
    
    # the inverse of matrix of A was not computed.
    # So that, we must computed for it.
    matrixData <- A$get()
    inversedMatrix <- solve(matrixData, ...)
    A$setInversedMatrix(inversedMatrix)
    inversedMatrix
}


# unit test
A = makeCacheMatrix(matrix(c(1:4), nrow = 2, ncol = 2))

# The first time, inverse of maxtrix A was not computed
cacheSolve(A)

# For later, the function cacheSolve will get a cached data, instead of recomputing
# inverse of matrix A
cacheSolve(A)
cacheSolve(A)
cacheSolve(A)