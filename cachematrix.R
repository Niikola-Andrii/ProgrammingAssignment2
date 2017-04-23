# The two functions together provide a means to cache data 
# rather then recompute the data. The functions here are 
# almost identical to those used in the "Caching the Mean"
# example given in Programming Assignment 2 description.

# Creates a list containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of matrix.
# 4. get the value of the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) 
    {
    m <- NULL
    
    set <- function(y) {x <<- y; m <<- NULL}
    get <- function() x
    
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    
    list(set = set,get = get, setSolve = setSolve, getSolve = getSolve)
    }

# The following function calculates the inverse of 
# matrix created with the above function. However, 
# it first checks to see if the inverse has
# already been calculated.
cacheSolve <- function(x, ...) 
    {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()

    if(!is.null(m))
        {
        message("2. GET cached data")
        return(m)
        }
  
    message("1. SET cached data")
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m);
    m;
    }

# a Test Run
# ----------
#
# > a = makeCacheMatrix(matrix(c(1,2,1,2,5,0,3,3,8),nrow = 3,ncol = 3))
# > cacheSolve(a)
# 1. SET cached data
#       [,1] [,2] [,3]
# [1,]  -40   16    9
# [2,]   13   -5   -3
# [3,]    5   -2   -1
# > cacheSolve(a)
# 2. GET cached data
#       [,1] [,2] [,3]
# [1,]  -40   16    9
# [2,]   13   -5   -3
# [3,]    5   -2   -1