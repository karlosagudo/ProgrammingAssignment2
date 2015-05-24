## Put comments here that give an overall description of what your
## functions do

## @x: a square invertible matrix
## return: a list containing functions to set, get, set the inverse, and get the inverse of the matrix
## I will use this function as input on the cache solve
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) {
      inv <<- inverse
    }
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv , getinv = getinv)
}


## @x: a square invertible matrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv = x$getinv()
    
    if(!is.null(inv)) {
        message("getting the cache data")
        return(inv)
    }
    
    #we calculate the solution
    squaremat = x$get()
    inv = solve(squaremat, ...)
    
    # we cache for next result
    x$setinv(inv)
    
    #finally we return the inverse
    inv
}

#function to do the test
test = function(squareMat) {
    listMat = makeCacheMatrix(squareMat)
    message("First Time")
    cacheSolve(listMat)
    message("SecondTime")
    sol <- cacheSolve(listMat)
    sol
}

r = rnorm(100)
mat1 = matrix(r, nrow=10, ncol=10)
solution <- test(mat1)



