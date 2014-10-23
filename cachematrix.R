## Caching the inverse of a matrix could be important, especially when we cannot take advantage of a powerful processors or RAM.
## The following functions could then be saved to be used whenever necessary, since an empty matrix is passed as main argument,
## in order to be specified by the user, depending on the project o purpose.

## This function will create a matrix, it will set and get its value, and will set and get its inverse

makeCacheMatrix <- function(x = matrix()) {			# the function takes an empty matrix as main argument
        i <- NULL								# NULL is the default value of "i" before the inverse is been calculated
        set <- function(y) {						# a value "y" is attributed to the matrix
                x <<- y							# and cached with the <<- operator
                i <<- NULL						# in the same way, NULL is assigned to its inverse with the <<- operator
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve		# this function will be used by cacheSolve to store the inverse in memory
        getinverse <- function() i					# we set a function to get the inverse of the matrix, which has now an empy argument
        list(set = set, get = get,					# it creates a list including all the four functions used in these two main closures
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will check whether the inverse of the matrix created with the above function has been calculated. 
## If the inverse has not been calculated before, it will calculate and return its value.

cacheSolve <- function(x, ...) {					## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()						
        if(!is.null(i)) {						 
                message("getting cached data")			# if the inverse has been already calculated cacheSolve will say that
                return(i)						# its value is in memory and it will return it
        }
        matrix <- x$get()						# if the inverse has not been calculated - i.e. 'is.null(i)==T',
        i <- solve(matrix, ...)					# 'i' will be calculated in this instance,
        x$setinverse(i)							# then it will be cached through the 'setinverse' function, 
        i									# and returned
}

