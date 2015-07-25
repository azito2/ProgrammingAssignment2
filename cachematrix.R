## This function is largely based off of the structure of the example,
## but I have tested it on several matrices with good results.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
        #"inv" will be assigned to the inverse, this ensures 
        #it will be clear of previous values
    set <- function(y) {
        x <<- y
        inv <<- NULL
            #the set function allows you to change the matrix you have paired
            #to a particular object. For example:
            #RiRa <- makeCacheMatrix(c(2,4,6,8)2,2) makes a 2x2 matrix 
                #[,1]=2,4 [,2]=6,8
            #if you enter RiRa$set(c(1,3,5,7,9,11,13,15,17)3,3)
            #the matrix paired to RiRa will now be 3x3 odd numbers.
            #inv <<- NULL clears inverse as before, in parent environment.
            }
    get <- function() x
            #the get function simply returns the matrix x
    setinverse <- function(solve) inv <<- solve
            #setinverse does two things:
            #   1) uses solve() to assign the inverse of the matrix to "inv"
            #   2) pushes inv to the global environment using <<-
    getinverse <- function() inv
            #getinverse returns "inv" the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
            #all these four functions must be preserved in a list to be used
            #in the function below
}


## This function will use the nested functions above to either return or 
## calculate the inverse of the matrix paired to an object with the 
## function above.

cacheSolve <- function(x, ...) {
        #The major argument here, x, should be a matrix, but the function can take others
    inv <- x$getinverse()
        #If x is an object assigned to makeCacheMatrix, getinverse should be a value 
        #assigned within it. This designates the result of getinverse as "inv"
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
            #If the inverse is stored, i.e the result of calling getinverse is not NULL,
            #this will return the stored value following a message.
    }
    data <- x$get()
            #If the inverse was not found stored, this will assign the stored matrix to "dat"
    inv <- solve(data, ...)
            #This assigns the inverse of "dat" to "inv"
    x$setinverse(inv)
            #This sets the invers as "inv" (to be returned by x$getinverse)
    inv
    ## This returns a matrix that is the inverse of x
}
