## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <-  NULL
        set <-  function(y) {
                # `<<-` to assign a value to an object in an environment 
                # different from the current environment
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setInverse = function(inverse) inv <<- inverse 
        getInverse = function() inv
        list(set=set, 
             get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInverse()
        if (!is.null(inv)){
                # get it and skips
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setInverse(inv)
        return(inv)
        
}
