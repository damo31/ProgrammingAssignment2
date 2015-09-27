## Calculation of matrix inverse requires costly computation. Caching inverse of a matrix can help avoid repeated
## computation of the same matrix. These functions will help cache the inverse of a matrix.

## makeCacheMatrix produces a list containing a function that:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of inverse of a matrix
## 4. get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of a matrix. It checks whether the inverse has already been computed. If yes, it 
## gets the result and skips the computation. If no, it goes ahead and computes the inverse, and sets the value in the
## cache through the setinverse function. The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
     }
     data <- x$get()
     inverse <- solve(data)
     x$setinverse(inverse)
     inverse
}

## Return a matrix that is the inverse of 'x'.
## I use a 3x3 matrix to test the above functions.
## > x = rbind(c(1, 5/7, 5), c(-4/7,1,3), c(6,2,1))
## > m = makeCacheMatrix(x)
## > m$get()
##           [,1]      [,2] [,3]
##[1,]  1.0000000 0.7142857    5
##[2,] -0.5714286 1.0000000    3
##[3,]  6.0000000 2.0000000    1
## > cacheSolve(m) : There is no previous calculation of the same matrix before. Hence, no cached data.
##           [,1]        [,2]        [,3]
##[1,]  0.1821561 -0.33828996  0.10408922
##[2,] -0.6765799  1.05650558  0.21338290
##[3,]  0.2602230 -0.08327138 -0.05130112
## > cacheSolve(m) : The same calculation a second time now.
## getting cached data
##           [,1]        [,2]        [,3]
##[1,]  0.1821561 -0.33828996  0.10408922
##[2,] -0.6765799  1.05650558  0.21338290
##[3,]  0.2602230 -0.08327138 -0.05130112





