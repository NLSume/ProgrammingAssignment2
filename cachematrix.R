# makeCacheMatrix will create a special matrix which will be used for cacheSolve() function.
# cacheSolve() function will create an inverse of a matrix and cache the created inverse matrix.
# If the same matrix is called from the cacheSolve() function, the cached inverse matrix will 
# be returned instead of running the slove() function again. 

## makeCahceMatrix will create a special matrix function.
makeCacheMatrix <- function(x = matrix()) {
        #Initialize inverse matrix to NULL.
        inv <- NULL
        
        #This set() funtion is to create the special matrix using matrix$set().
        set <- function(y){
                x <<- y
                #When the special matrix is created using matrix$set(), the inverse
                #matrix is also set to NULL.
                inv <<- NULL
        }
        #get() funtion will return the initial matrix. 
        get <- function() x
    
        #setinverse() is a function that set a particular value to inverse matrix.
        #The argument value inverse is pushed to gobal environment. 
        setinverse <- function(inverse) inv <<- inverse
    
        #getinverse() function is to get an inverse matrix. 
        getinverse <- function() inv
      

        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=setinverse)
}


# cacheSove() will take the special matrix created by makeCacheMatrix() function
# as an argument and will produce the inverse matrix. Then, it will chache the result.
# If the inverse matrix has already been calculated (and the matrix has not changed), 
# then the cacheSolve() should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        #When the cacheSolve() is invocked, getinverse() is called and the result is assigned to inv.
        #If a matrix was called for the first time, the value will be NULL.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                #If the inv value is not NULL, it will return the cached value.
                message("getting cached data")
                return(inv)
        }
        
        #If the data was not NULL it will get the data and make inverse of the matrix.
        data <- x$get()
        inv <- solve(data, ...)
        #After the inverse matrix is made, it is assigned to the inv in the global environment.
        x$setinverse(inv)
        inv
}
