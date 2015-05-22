makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache 
        ## its inverse
        ## Args:
        ##   x is a square matrix. It´s supposed that the matrix is invertible      
        ## Returns:
        ##   A special "matrix" containing the following 4 functions: 
        ##   set, get, setInverse, getInverse   
        ##
        m <- NULL
        set <- function(y) {
                ## This function set the value of the matrix
                ## Args:
                ##    y is the matrix to calculate the inverse
                ## Returns:
                ##    x return the matrix to the upper environment
                ##    m If the matrix has changed, the inverse is set to NULL
                x <<- y         
                m <<- NULL      
        }
        get <- function() {
                ## This function get the value of the matrix
                ## Returns:
                ##    x return the matrix to the calling environment
                x
        }        
        setInverse <- function(inverse) {
                ## This function set the value of the inverse of the matrix
                ## Args:
                ##    inverse is the inverse of the matrix (cached)
                ## Returns:
                ##    m return the value of the inverse
                m <<- inverse
        }        
        getInverse <- function() {
                ## This function get the inverse of the matrix (cached)
                ## Returns:
                ##    m return the value of the inverse
                m
        }        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cachesolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" 
        ## returned by makeCacheMatrix
        ## Args:
        ##    x is the matrix to calculate the inverse
        ## Returns:
        ##    m The inverse of the matrix
        m <- x$getInverse() 
        if (!is.null(m)) { ## If m is not NULL it return the cached inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ##Compute the inverse of the matrix obtained by x$get()
        x$setInverse(m)
        m
}