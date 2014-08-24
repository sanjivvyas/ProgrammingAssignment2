## Inverse of matrix is a resource consuming and time consuming costly operation. If we can cache inverse of a matrix
## rather than compute over again then it can save time.
## These functions could be used to achieve this.

## makeCacheMatrix sets and gets the value of a matrix and sets and gets value of an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  inv1 <- NULL
  set <- function(y) {
    x <<- y
    inv1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv1 <<- inverse
  getinverse <- function() inv1
  list(
    set=set, 
    get=get, 
    setinverse=setinverse, 
    getinverse=getinverse
    )
    
}


## cacheSolve computes the inverse of a matrix. It checks first if the inverse of matrix already exists. If does then 
## it does not compute the inverse. If it does not then it computes the inverse.
## There is an assumption that matrix are invertible.

cacheSolve <- function(x, ...) {
  inv1 <- x$getinverse()
  if(!is.null(inv1)) {
    message("getting cached data.")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data)
  x$setinverse(inv1)
  inv1
  
}
