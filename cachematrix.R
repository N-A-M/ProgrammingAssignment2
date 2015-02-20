## Put comments here that give an overall description of what your
## functions do
## the functions below are a set of two functions that collectivly 
## calculate the inverse of a matrix and saves it in the cache.
## when a request is posted to calculate the inverse a condition checks 
## if the inverse was recently calculated and saved in the cache.
## for one makecachematrix object only one matrix and its inverse 
## are allowed in the cache at a time, new matrix value overrides old one

## Write a short comment describing this function
## this function returns a list of 4 functions
## set sets the matrix x in the cache and resets its inverse to NULL
## get gets the matrix x from the cache
## setinv sets the inverse of the matrix in the cache
## getinv gets the inverse of the matrix from the cache.
## the calling code needs to provide a non singular square matrix 
## and call the functions as the code example :
## myMatrix <- makeCacheMatrix(matrix (c(2,-5,1,1,-3,1,1,0,-1),3,3))
## cacheSolve(myMatrix)
## subsequent call to cacheSolve will print "getting cached data"
## and prints the inverse
#############################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## this if condition is to prevent setting the matrix 
        ## and resetting the inverse
        ## if the new value is the same as the old one
        if ( is.null(x) || x!= y ){
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## this function receives list of access fucntions for 
## a matrix object 
## it then uses it to get the cached inverse.
## if the inverse is not cached
## then the function calculates the inverse 
## and then it calls makecachematrix to set the matrix inverse
## in the cache
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}


