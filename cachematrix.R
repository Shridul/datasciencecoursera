## Put comments here that give an overall description of what your
## functions do
#Prog Assignment-2
## Write a short comment describing this function
## this matrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <-NULL #stores the inverse of matrix
set<-function(y){
    x<<-y
    inv<<-NULL
}
get<-function()x  ##returns ther value of the matrix

setinv<-function(inverse) inv<<-inverse ##assigns value of inv in parent environment
getinv<-function() inv ##gets the value of inv where called
list(set=set,get=get,setinv=setinv,getinv=getinv)##to create a list to refer them

}


## Write a short comment describing this function
#this function will find the inverse of the matrix returned by makeCacheMatrix
## if the inverse is already calculated & matrix hasn't changed then cacheSolve
## will retrieve the inverse
cacheSolve <- function(x, ...) {
     ## return a matrix that is inverse of 'x'
     inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinverse(inv)
    inv
     }
