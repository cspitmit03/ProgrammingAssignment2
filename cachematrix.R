## The first function will store and cache a matrix.
## s and x are stored in the global environment to reset variables
## everytime it is called
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
## This is a boolean check to see if the matrix being stored 
## is square in order to solve.  If it isn't it stops and askes user
## to try again
        if((ncol(get()) != nrow(get())) == "TRUE") {
                message("matrix is not square try again")
        }
## store solve function to call later
        setsolve <- function(solve) s <<- solve
## retrieve solve function to call later
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The second function will call and compute the inverse of a matrix.
cacheSolve <- function(x, ...) {
## Call solve function from makeCacheMatrix        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
## store matrix in new variable from makeCacheMatrix for computation
        data <- x$get()
## compute inverse
        s <- solve(data, ...)
        x$setsolve(s)
## print new matrix inverse
        message("here is a new matrix inverse solution; not cached")
        s
}