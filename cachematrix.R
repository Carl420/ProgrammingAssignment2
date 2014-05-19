# Inverse of a Matrix - 2014 May 19

# The inverse of any invertible square matrix is computed
# using two functions:
#  cashSolve computes the inverse of the "special" matrix returned by 
#  makeCacheMatrix.

# If the inverse has already been calculated (and the matrix has not changed), 
# then the cashSolve function retrieves the inverse from the cache.

# An example is included for illustrative purposes.  The secon time the inverse
# is computed the message "getting cached data" appears in the console.

makeCacheMatrix <- function(x ) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        #print(m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
mat <-matrix(c(1, 4, -2, 5, -3, 6, 3, 7, -11), nrow = 3, ncol = 3)
x <- makeCacheMatrix(mat)
cacheSolve(x)
cacheSolve(x)
