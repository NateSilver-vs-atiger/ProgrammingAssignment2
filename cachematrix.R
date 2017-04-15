## It took a while but I understand it now!
## 

## This function defines a list of 4 functions all based on the input matrix


makeCacheMatrix <- function(x = matrix()) { 
        I <- NULL                       ##Sets I intially as Null
        set <- function(y) {            ##Defines x, I enviro specific
                x <<-  y
                I <<- NULL
        }
        get <- function() x             ##retrieves x
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get, ##Defines the list based on the 4 functions
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}




## This function makes use of the four functions defined by the first part to
## either solve the inverse of a matrix or retrieve a cached value

cacheSolve <- function(x, ...) {
        I <- x$getinverse()     #getinverse from MakeCache now stored as I
        if(!is.null(I)){        #checks to see if already stored, if not returns            
                message("getting cached data")
                return(I)
        }
        data <- x$get()         #else, solves and returns.
        I <- solve(data, ...)
        x$setinverse(I)
        I
        ## Return a matrix that is the inverse of 'x'
}





