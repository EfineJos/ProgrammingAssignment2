## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Setting the inverse attribute
        i <- NULL
        
        ## Set matrix
        set <- function(matrix) {
                x <<- matrix
                i <<- NULL
        }
        
        ## Get matrix
        get <- function() {
                ## Return the matrix
                x
        }
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        ## Create a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
        ## Return inverse matrix of 'y'
        x <- y$getInverse()
        
        
        if( !is.null(x) ) {
                message("getting cached data")
                return(x)
        }
        
        ## Get the matrix from our object
        data <- y$get()
        
        ## Calculate the inverse using matrix multiplication
        x <- solve(data) %*% data
        
        ## Set the inverse to the object
        y$setInverse(x)
        
        ## Return the matrix
        x
}

## Testing the functions with an square matrix
g <- makeCacheMatrix(matrix(1:4,2,2))
g$get()
g$getInverse()
cacheSolve(g)