## These functions allow users to calculate inverse of a matrix. If it is 
## already calculated, then it returns the cached value of the inverse

## makeCacheMatrix creates a special matrix which has the capability to cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        s<- NULL
        set<- function(y){
                x<<-y
                s<<-NULL
        }
        get<- function() x
        setinverse<- function(solve) s <<- solve
        getinverse<- function() s
        list(set=set, get= get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve checks if the inverse has already been calculated, if yes, it 
## returns cached value, else it calculates and return inverse of the special 
## vector

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
