## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Following functions enables computing and caching inverse of a matrix.
##
## Assumption - For this assignment, assume that the matrix supplied is always invertible.
##
## Usage (example) - 
## > source(cachematrix.R)
## > cm <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(cm)


## Function name: makeCacheMatrix()
## This function creates a special object which stores (as a part if its env)
## 1) original matrix
## 2) inverse of a matrix (used as cache)
## Function Parameters
## 1) x - type:matrix - original matrix
## The function returns a list of functions
## 1) set() - set the value of the matrix
## 2) get() - get the value of the matrix
## 3) setinverse() - set the inverse matrix
## 4) getinverse() - get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the matrix inverse to NULL
    x_inv <- NULL
    
    ## set original matrix
    set <- function(newmatrix) {
        x <<- newmatrix
        x_inv <<- NULL
    }
    
    ## get original matrix
    get <- function() {
        x
    }
    
    ## set inverse of the matrix
    setinverse <- function(inv) {
        x_inv <<- inv
    }
    
    ## get inverse of the matrix
    getinverse <- function() {
        x_inv
    }
    
    ## return list of getter and setter functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function name cacheSolve()
## This function returns the inverse of the matrix. For optimization
## it returns cached value of inverse if available or 
## calculates and caches the inverse and returns the inverse.
## Function parameters
## 1) x - type: list - special matrix object created using makeCacheMatrix()
## Function returns
## 1) inverse of the matrix

cacheSolve <- function(x, ...) {
    ## get inverse of the matrix
    inv <- x$getinverse()
    
    ## check if cache is valid (has inverse)
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    message("calculating and caching inverse")
    ## get original matrix
    mat <- x$get()
    
    ## calculate and cache the inverse if matrix
    newinverse <- solve(mat, ...)
    x$setinverse(newinverse)
    
    ## return inverse of the matrix
    newinverse
}

