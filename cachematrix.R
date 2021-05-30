## Put comments here that give an overall description of what your
## functions do
## makecacheMatrix is able to set a matrix and its inverse on the variabale x and inv respectively
## cacheSolve is able to checks if there is an inverse set in inv, if there is not value in inv, 
## then this function calculates the inverse of x an det it in inv


## Write a short comment describing this function
## You have to create a new variable that will contain 
##the list of functions that makeCacheMatrix contains 
## matrix <- makeCacheMatrix()
## 1. matrix$set_m saves a matrix on a variable x and resets the 
## inv variable in case it has a inverse saved in it.
## 2. matrix$get_m print the matrix that you saved in x
## 3. matrix$set_inv_m Save the inverse of the matrix x in the variable inv
## 4. matrix$get_in_m print the inverse that was saved in the variable inv 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set_m <- function(y){
    x <<- y
    inv <<- NULL
  }
  get_m <- function(){
    x
  }
  set_inv_m <- function(i) inv <<- i
  get_inv_m <- function() {
    inv
  }
  list(set_m=set_m, get_m=get_m, set_inv_m=set_inv_m, get_inv_m=get_inv_m )

}


## Write a short comment describing this function
## You have to put the variable where you saved makeCacheMatrix
##for example cacheSolve (matrix)
## Then the function will check if the inverse of x is already set on the variable inv 
##of the environment of makecacheMatrix.
## 1. In case there is an inverse saved in inv, then 
## the function will print the inv variable
## 2. In case there is not an inverse saved in inv, then cacheSolve will calculate the 
## inverse of x, save it in the variable inv and print the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv_m()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)  
  }
  else{
    mx <- x$get_m()
    imx <- solve(mx)
    x$set_inv_m(imx)
    imx
  }
        ## Return a matrix that is the inverse of 'x'
}
