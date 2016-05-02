## The first function creates a list of functions that set and get a matrix and its inverse.
## The second function is passed the list from the first and attempts to calculate and set its inverse.  If the inverse is already set, the cached value is used.
## The special assignment operator, <<-, is used to change the value associated with x and cachedInv. This operator looks back in enclosing environments for an environment that contains the symbol total and when it finds such an environment it replaces the value, in that environment, with the value of right hand side. If the global or top-level environment is reached without finding the symbol total then that variable is created and assigned to there.
## When makeCacheMatrix is invoked, it will create a matrix x and return a list containing functions to set/get x and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL ## initialize inverse
        
        ## set x in parent env with the desired value, if inverse is already set, set it to NULL!
        set <- function(user_value = matrix()) {
                x <<- user_value 
                cachedInv <<- NULL
        }
        
        get <- function() x
        
        ##set inverse variable in parent env to desired value and return the value.
        setInverse <- function(invVal) {
                cachedInv <<- invVal 
                return(cachedInv)
        }
        
        getInverse  <- function() cachedInv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given the list variable from the first function, first check to see if there's already a cached inverse and, if so, return it.
## Otherwise attempt to solve its inverse and set/return it.

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
        
        ## check if there's something there already
        calculatedInverse <- x$getInverse() 
        
        ##check if there's a cached value AND it's a matrix
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
                message("Hurray! We found cached data and it is the inversed matrix you are looking for!")
                return(calculatedInverse)
        }
        
        ## otherwise get the matrix
        matrixToSolve <- x$get()  
        
        ## try to solve the matrix and catch errors and warnings
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, simpleWarning=function(w) {
                message("Opps. This may not be the result you are looking for :-(")
                message(w)
        }, simpleError=function(e) {
                message("Something went terribly wrong solving your matrix")
                message(e)
                message("\n")
        })
        
        ## whatever the case, set the value of the inverse (NULL if something went wrong)
        message("Setting the value of inverse to:") 
        x$setInverse(calculatedInverse)
}