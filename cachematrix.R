## The below functions are used to calculate the inverse of a matrix and cache
## the result to avoid running the same inverse calculation each time the inverse
## is needed.

## Function stores the argument matrix in the function environment and caches 
## it's inverse for future use.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL  #Initialize object to hold inverse matrix
        
        set <- function(y) {
                # Re-sets the argument (x) and deletes inverse result in the 
                # parent function environment
                x <<- y
                inverse <<- NULL 
        }
        get <- function() x  #Returns value of x
        setinverse <- function(inv) inverse <<- inv  #Caches value of inverse of x
        getinverse <- function() inverse  #Return cached inverse
        
        # Return list of functions to get and set matrix and inverse when calling makeCacheMatrix
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Function retrieves the cached inverse of a matrix or computes and caches 
## the inverse of the matrix

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()   #read matrix cache for inverse value
        
        # Check if inverse matrix has already been cached and return cached result
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()             #Read data from matrix cache to compute inverse
        inverse <- solve(data, ...) #Compute inverse
        x$setinverse(inverse)       #Store inverse in matrix cache
        inverse                     #Return Inverse result
}