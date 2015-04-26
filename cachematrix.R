makeCacheMatrix <- function(x = numeric()) {   
    m <- NULL                                    
    set <- function(y) {                       ## set is assigned a function that initializes the x and m variables
        x <<- y                                    
        m <<- NULL                                 
    }                                           
    get <- function() x                        ## get is assigned a function that returns the original matrix
    setinverse <- function(solve) m <<- solve  ## setinverse is assigned a function that stores the solve function in m
    getinverse <- function() m                 ## getinverse is assigned a function that returns the value stored in m
    list(set = set, get = get,                 ## makeCacheMatrix returns a list containing set, get, setinverse,
         setinverse = setinverse,              ## and getinverse, to be used later with the cacheSolve function.
         getinverse = getinverse)    
}                                   
                                    
cacheSolve <- function(x, ...) {
    m <- x$getinverse()                 ## m is assigned the value contained in the getinverse element of x
    if(!is.null(m)) {                   ## if m is not null (meaning that the inverse has already been calculated & stored)
        message("getting cached data")  ## print a line indicating you are retrieving the already-calculated inverse
        return(m)                       ## return the value stored in m (the previously calculated inverse of the matrix)
    }                                   
    data <- x$get()                     ## else, store the matrix in 'data' (retrieved from the get element in x)
    m <- solve(data, ...)               ## run the solve function on data (calculate inverse of the matrix), store it in m
    x$setinverse(m)                     ## store that result in the setinverse element of x (so it won't need to be calulated again)
    m                                   ## return m, which now contains the inverse of the matrix
}