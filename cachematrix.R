## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInvM <- function(solve) invM <<- solve
        getInvM <- function() invM
        list( set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInvM()
        if(!is.null(invM)) {
                message("Getting cached data ...")
                return(invM)
        }
        mat <- x$get()
        invM <- solve(mat)
        x$setInvM(invM)
        invM
}
