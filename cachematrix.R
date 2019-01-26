    ##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
    #get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
    #can cache its own object. 
    
    makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
            x <<- y
            invMatrix <<- NULL
        }
        
        getMatrix <- function() x                              
        setInverse <- function(inverse) invMatrix <<- inverse  
        getInverse <- function() invMatrix                     
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
        
    }
    
        ## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
        
    cacheSolve <- function(x, ...) {
        
                invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       
            message("Getting Cached Invertible Matrix") 
            return(invMatrix)                           
        }
        
         
        MatrixData <- x$getMatrix()                     
        invMatrix <- solve(MatrixData, ...)             
        x$setInverse(invMatrix)                         
        return(invMatrix)                               
    }
