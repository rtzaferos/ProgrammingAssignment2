## The following function (makeCacheMatrix) creates a special matrix 
#which is really a list of functions that:


##
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix
# 4 get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL               #Initialize the inverse value (s) in local environment
        set <- function(y) {     
                x <<- y         #sets the value of the matrix in the parent environment
                s <<- NULL      #sets the inverse value (s) to NULL in the parent environment
        }
        get <- function() x                     #gets the value of the matrix
        setsolve <- function(solve) s <<- solve #sets the value of the inverece of the matrix in the parent environment
        getsolve <- function() s                #gets the value of the inverse of the matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
        

}


## This function calculates the inverse of the matrix created by the "makeCacheMatrix" function.
#First it checks if an inverse value is already calculated and if so gets the value from the cache.
#Othewise it calculates the inverse value and then sets it in the cache,

cacheSolve <- function(x, ...) {
        
        s <- x$getsolve()
        if(!is.null(s)) {                          # checks if an inverse value is already calculated
                message("getting cached data")
                return(s)                          # if so gets the value from the cache
        }
        data <- x$get()
        s <- solve(data, ...)                   # Othewise it calculates the inverse value 
        x$setsolve(s)                           # sets it in the cache
        s                                       ## Return a matrix that is the inverse of 'x'
}
        
        
}
