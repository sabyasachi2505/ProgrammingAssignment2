##The following two functions can be used to create a matrix and calculate its
##inverse with the ability to cache the inverse matrix if the original matrix
##remains unchanged.

##This function creates a list with 4 functions get(), set(), setinverse() & getinverse().
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##set() function enables the user to enter a valid matrix
        set <- function(y){
                if(class(y)!="matrix"){
                        message("Please enter a valid matrix")}
                else{   x <<- y
                        i <<- NULL}
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##The cacheSolve() function is used to calculate the inverse of the matrix
##created using the makeCacheMatrix() function. This function returns a cached
##value of the inverse matrix if the original matrix remains unchanged.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if(!is.null(i)){
                message("getting cached data")
                #the cached value of the inverse matrix is returned
                return(i)}
        
        data <- x$get()
        
        #inverse is calulated newly
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
