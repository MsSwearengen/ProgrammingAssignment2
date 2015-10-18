
## The following function defines a list of functions for the argument x, a matric
##it also contains a object inv which it sets to NULL if not defined by a previous run of cacheSolve


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x
        setinv <- function(solve) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




# This function first scopes to see if inv is already defined by calling 
#getinv from the function makeCacheMatrix.

cacheSolve <- function(x, ...) {

        inv <- x$getinv()
##the next part checks if inv is a value or null (if hasn't been calculated before).  If it is already defined will get from cache
        
		if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
#otherwise will calculate inv from scratch and save it *using function setinv from inside 

        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
