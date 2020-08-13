## When working with large data sets, we must take care of how many 
## mathematical operations we do with our values. Careless steps in 
## our code can lead to a huge computational time-consuming. Knowing 
## this, the next 2 functions make it ## possible to store a matrix 
## and its inverse to cache, eliminating the need for recalculations.

# ******************************************************************** #

## return1 <- makeCacheMatrix(arg1)
## arg1: any square invertible matrix
## return1: a list of 4 elements

## The "makeCacheMatrix" function creates a list of 4 elements. 
## Each element is a function with different purposes: 
## setMAT: allows the user to set the matrix
## getMAT: prints the matrix
## setINV: set the inverse of the square matrix by using "solve" R function
## getINV: prints its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMAT <- function(y) {                    # if user provides, setMAT <- arg1
    x <<- y                                  # if not, setMAT <- NULL
    m <<- NULL
  }
  getMAT <- function() x                    # print the stored matrix
  setINV <- function(solve) m <<- solve     
  getINV <- function() m                    # print the inverse matrix
  list(setMAT = setMAT, getMAT = getMAT,    # list with 4 arguments(functions)
       setINV = setINV,
       getINV = getINV)

}


# ******************************************************************** #


## return1 <- cacheSolve(arg1)
## arg1: a list of 4 elements created by "makeCacheMatrix" function
## return1: a inverse matrix

## If the "setINV" of arg1 is empty, "cacheSolve" calculates the 
## inverse of the matrix. If it is not, the function
## return the matrix already stored in arg1.


cacheSolve <- function(x, ...) {
  m <- x$getINV()
  if(!is.null(m)) {                   # inverse matrix already stored
    message("getting cached data")
    return(m)
  }
  data <- x$getMAT()
  m <- solve(data, ...)               # calculates the inverse using solve()
  x$setINV(m)
  m
}


## Example: 
## 1) matrix_stored <- makeCacheMatrix(matrix(1:4,2,2))
## 2) Print the matrix: matrix_stored$getMAT()
## 3) Getting its inverse form: cacheSolve(matrix_stored)
## 4) Print the inverse: matrix_stored$getINV()

## In the case where you already know the inverse matrix:
## 3') matrix_stored$getINV(inverse_matrix)


