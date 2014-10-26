###
### This two functions calculates inverse matrix and cache it.
### If the same matrix is use, then the inverse its not re-calculate  
### and the function return cached matrix.
###

## This function transforms a matrix into a data structure. 
## This data structure is a list that contains a cached matrix 
## and some methods to manipulate it. 

makeCacheMatrix <- function(x = matrix()) {
		CacheInverseMatrix <- NULL
        
		setMatrix <- function(y){
                x <<- y
                CacheInverseMatrix <<- NULL
        }
		
        getMatrix <- function(){ 
		  x
		}
		
        setInverseMatrix <- function(solve){
			CacheInverseMatrix <<- solve
        }
        
		getInverseMatrix <- function() {
			CacheInverseMatrix
        } 	
		
        list( setMatrix = setMatrix
		     ,getMatrix = getMatrix
			 ,setInverseMatrix = setInverseMatrix
			 ,getInverseMatrix = getInverseMatrix 
			)
}

## This function receives a data structure that 
## represents a matrix and its  returns the inverse matrix 

cacheSolve <- function(x, ...) {
        
		InverseMatrix <- x$getInverseMatrix()
		
        if(!is.null(InverseMatrix)) {
            message("getting cached inverse matrix")
            return(InverseMatrix)
        }
        
		data <- x$getMatrix()
        
		InverseMatrix <- solve(data, ...)
        
		x$setInverseMatrix(InverseMatrix)
		
        InverseMatrix	
}
