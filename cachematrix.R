## These two functions, makeCacheMatrix and cacheSolve, work together
## to solve invertible matrices and then cache the result such that after
## solving the first time, the inverse matrix can be retrieved from the
## cached value without having to repeatedly solve for it.

## First makeCacheMatrix is called.  This returns a list of functions that
## provide for setting and getting the matrix to be solved (set, get) and
## for setting (setInverse) and getting (getInverse) the inverse matrix 
## in the cached object 'im'.

makeCacheMatrix <- function(x = matrix()) {
        ## Takes a matrix x with default of empty matrix
        ## Returns list of functions as described above
        
        ## Inverse matrix im NULL until cached
        im <- NULL
        
        ## Functions to get and set the matrix to be solved
        set <- function(y) {
                x <<- y
                im <<- NULL  # reset im to NULL
        }
        get <- function() x
        
        ## Functions to set and get the inverse matrix in the cache
        setInverse <- function(inverse) im <<- inverse
        getInverse <- function() im
        
        ## List functions to make available to cacheSolve
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## When cachSolve is called on an object returned by makeCacheMatrix, it 
## first checks if the inverse matrix has already been cached and returns
## if so.  If not, it gets the matrix, solves it, puts it in the cache and
## returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## ... optional additional arugments passed to solve()
        
        ## First check if already solved and cached
        im <- x$getInverse()
        if(!is.null(im)) {
                message("Getting cached data")
                return(im)
        }
        
        ## If not, get the matrix, solve, 
        ## and cache the resulting inverse matrix
        m <- x$get()
        im <- solve(m, ...)
        x$setInverse(im)
        
        ## Finally return the inverse matrix
        im
}
