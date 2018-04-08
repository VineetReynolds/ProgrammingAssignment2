## A set of functions to help with computing inverses of matrixes
## involving the use of a cache to eliminate repeated computations
## if the inverse for a matrix was computed.

## Create a 'special' matrix, given a matrix 'm'.
## The return value can be used as an input to the cacheSolve function
## The matrix returned contains 4 functions -
## get - to get underlying raw matrix
## set - to set the matrix in it's raw form
## getInverse - retrives a cached copy of the inverse of the matrix, 
##                 and null if the inverse was not computed earlier 
## setInverse - sets the cached inverse of the matrix to the parameter 
makeCacheMatrix <- function(x = matrix()) {
    ## Return a list of functions that wraps operations on the matrix parameter
    
    ## 'i' holds the cached inverse of the matrix 'm'
	i <- NULL
    ## Function to set the wrapped matrix to a parameter 'y'
	set <- function(y) {
        ## Use the super-assignment operator to search the environments
        ## and set the raw matrix to 'y'.
        ## Also reset the inverse 'i' to NULL
		x <<- y
		i <<- NULL
	}
    ## Function to get the wrapped matrix
	get <- function() {
        ## Return the raw matrix 'm' from it's environment
		x
	}
    ## Function to return the inverse
	setInverse <- function(inv) {
        ## Set the inverse to the param 'inv'.
        ## Uses super assignment to search for 'i' through environments.
		i <<- inv
	}
    ## Function to set the inverse
	getInverse <- function() {
        ## Return the inverse 'i' from it's environment
		i
	}
    ## Return a list containing these 4 functions
	list(set = set, get = get,
		getInverse = getInverse,
		setInverse = setInverse)
}


# Accept a 'special' matrix and return it's inverse.
# The function assumes a square invertible matrix.
# Additional parameters supplied to this function will be used as 
# parameters to the matrix 'solve' function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        ## get the cached inverse of the matrix
		inv <- x$getInverse()
		if(!is.null(inv)) {
            ## if cache hit, then return the cached inverse
			message("getting cached matrix")
			return (inv)
		}
        ## if cache miss, then compute the inverse from the original matrix
		data <- x$get()
		inv <- solve(data, ...)
        ## add the inverse to the cache
		x$setInverse(inv)
		inv
}