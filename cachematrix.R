## The two functions below allow you to calculate the inverse of a matrix
## and cache the value for later recall. This helps save computing resources
## when repeated calculation is needed.

## The makeCacheMatrix function is designed to store a matrix and 
## some other property of the matrix. For the purposes of this assignment it 
## is intended to store the inverse of the original matrix, however in theory
## it could store any value in the invM object.

## The cacheSolve function is designed to retrieve the inverse matrix if it has
## already been calculated and stored, or if no value is stored calculate the 
## inverse and cache it for later recall.



## makeCacheMatrix() creates a list of functions that will set and return the
## initial matrix and calculate and return the inverse of the matrix. The 
## invM variable stores the inverse matrix. The set function changes
## the value of the stored matrix.  After changing the matrix, it sets invM to
## NULL to ensure the inverse of the old martix is not retrieved when the 
## matrix value has changed.  The get function returns the stored matrix.  The
## setInvM function stores the inverse of the matrix in the invM object. The
## getInvM funtion returns the inverse matrix stored in invM.

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInvM <- function(inverseMat) invM <<- inverseMat
        getInvM <- function() invM
        list( set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## cacheSolve() will return the inverse of a matrix stored in a object created
## with the makeCacheMatrix function above.  The input x must be and object 
## created with makeCacheMatrix().  This function first checks to see if the 
## inverse has already been calcuated and stored.  If so it returns the 
## cached matrix inverse, and exits the function.  If the inverse has not been 
## stored previously or the values of the stored matrix have changed, it 
## calcuates the inverse, stores the value for later recall, and returns the 
## inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInvM()
        if(!is.null(invM)) {
                ## Return cached inverse
                return(invM)
        }
        ## Calculate new inverse
        mat <- x$get()
        invM <- solve(mat)
        x$setInvM(invM)
        invM
}
