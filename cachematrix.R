
##This function creates a special "matrix"
##object that can cache its inverse 


makeCacheMatrix <- function(x = matrix()) {

        ## first initiate the inverse to null         
        inver<-NULL
        
        # Here we create the set function to create the matrix
        set <-function(y){
                x<<-y
                inver<<-NULL
        }
        
        ## Here we return the created matrix
        get <- function() x
        
        ## Here we create the inverse of our matrix 
        setInverse <- function(i) inver <<- solve(x) 
        
        ## And now we get (return) the inverse 
        getInverse <- function() inver
        
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the 
## special "matrix" created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        ##Find if the inverse already calculated get t from the makecachematrix
        ## for this we use the getInverse function we created 
        inver <- x$getInverse()
        
        ##If it´s already calculated the it returns de inverse
        if (!is.null (inver)) {
                ## Do nothing 
                
        }
        else {
                ##When the inverse is null then this process calculates it and
                ##uses the setInverse function created on makecachematrix
                mat <- x$get()
                inver <- solve(mat, ...)
                x$setInverse(inver)  
        }
       

}