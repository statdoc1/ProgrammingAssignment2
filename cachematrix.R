## this system of 2 functions stores matrices and their inverses in memory in so that repeated 
## calls to invert the same matrix can be averted if the inverse exists.  
##      mat1 is assumed to be an invertible matrix (non-singular)
## calling the two functions with 
##      x1<-makeCacheMatrix(mat1)
##      cacheSolve(x1)
## provides the inverse to mat1 by either recalling from memory or recomputing
## 
## interesting note is that x1<-makeCacheMatrix(mat1) followed by cacheSolve(x1) and
## x2<-makeCacheMatrix(mat2) followed by cacheSolve(x2) (for another invertible matrix, mat2)
## can be run repeatedly without confusion of the values.



## makeCacheMatrix creates four functions, setmat, getmat, setinv, and getinv that are 
## contained in a list.  E.g., x1<-makeCacheMatrix(mat1) produces a list of these 4 functions.
## x1$getmat() returns mat1. x1$getinv() returns inverse of mat1 if it was stored already
## x1$setmat(mat0) will store a new value of mat1 and setinv will set its inverse.


makeCacheMatrix <- function(mat1 = matrix()) 
        {
    invmat1<- NULL
    setmat<-function(mat0) 
            {
            mat1 <<- mat0
            invmat1 <<- NULL
    }
    getmat<-function() mat1
    setinv<-function(minv) invmat1<<-minv
    getinv<-function() invmat1
    list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)
            
        
}


## cacheSolve produces the inverse of the matrix x.  The result of makeCacheMatrix is becomes the argument 
## for cacheSolve.  cacheSolve first checks to see if the inverse is already calculated and stored 
## in memory.  If it is this value is returned.  If not the inverse is computed and stored 
## back in memory for possible later use.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        mat2<-x$getinv()
        if(!is.null(mat2))
        {
                message("getting cached inverse")
                return (mat2)
        }
        mat3<-x$getmat()
        mat4<-solve(mat3)
        x$setinv(mat4)
        mat4
}
