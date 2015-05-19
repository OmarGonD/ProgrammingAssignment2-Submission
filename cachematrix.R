

## makeCacheMatrix stores an inversed matrix in it. 
## It stars as a NULL object, but after running cacheSolve (related function)
## "m" changes to stored the new inveresed cache.
## There are also other functions to set and get the matrix - argument or inversed - matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y            ## Set a new matrix "y" instead of matrix "x"
                m <<- NULL         ## Reset the inversed matrix to NULL, because now we have a new matrix in place
        }
        get <- function() x        ## Returns the matrix
        setmatrix <- function(matrix) m <<- matrix ## Can be used to 
        getmatrix <- function() m  ## Returns the inversed matrix.
        list( set = set, get = get, ## List all the functions to work with.
              setmatrix = setmatrix,
              getmatrix = getmatrix)
        
}


## cacheSolve checks if there is a cached solved Matrix. 
## If matrix is already cached, it just get it back from "makeCachedMatrix"
## Else: it gets the matrix from "makeCachedMatrix", and uses "solve()" to invers it.
## Finally it sets the inversed matrix in makeCachedMatrix ("x$setmatrix(inversed.matrix)")
## If we execute it one more time, m is no longer NULL, so it just returns "m".



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()  ## get matrix stored in cache.
        
        if (!is.null(m)) {
                message("Getting cached inversed - matrix")
                return(m)
        }
        
        matrix <- x$get() ## If the cached inversed matrix value is NULL,
                          ## then get it matrix used as argument and get it inversed.       
        
        
        
        inversed.matrix <- solve(matrix,...)
        x$setmatrix(inversed.matrix) ## Set the inversed matrix in cache
        inversed.matrix  
}
