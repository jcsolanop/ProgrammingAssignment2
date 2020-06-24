
##This function creates a special "matrix"
##object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        set <-function(y){
                x<<-y
                inv<<-null
        }
        ## create function  
        ##   ddjg
        get <- function() x
        setInverse <- function(inverse) inv <<- solve(x) 
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the 
## special "matrix" created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}