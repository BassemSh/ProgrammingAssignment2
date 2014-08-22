## Function to return the inverse in a special way
## without repeating the calculation if it was computed for the same object

## initialize the special matrix
makeCacheMatrix <- function(A = matrix()) {
        inv <- NULL
        set <- function(y) {
                A <<- y
                inv <<- NULL
        }
        get <- function() A
        setInverse <- function(invA) inv <<- invA
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
	  inv <- A$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }

	  matData <- A$get()
        inv <- solve(matData)
        A$setInverse(inv)
        inv
	
}

###################  Execution #####################
# source("cachematrix.R")
# I <- matrix(1:4,2,2)
# M <- makeCacheMatrix(I)
# cacheSolve(M)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# ###################################################

