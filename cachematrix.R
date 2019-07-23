## Put comments here that give an overall description of what your
## functions do


## Make Cache Matrix function is to take a matrix and set them in scoped varibles
## We are keeping the input matrix  in variable "x" 
## We are keeping the inverse of the matrix in variable "m"
## We created 4 methods i.e 
##    get   :- to return the value of x
##    set   :- to intialize variables for x and m
##    getInv:- to return m 
##    setInv:- to store inverse of x in m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getInv <- function() m 
  setInv <- function(inverse) m <<- inverse
  list(get=get, set=set,getInv = getInv,setInv = setInv)
}

## cacheSolve function is to take a matrix and return the inverse of the matrix if it is already caluclated else 
## calculate the inverse of the matrix if it is reversable and store it in cache
## we are determining if the matrix is inversible or not by get determinant of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getInv()
  if(!is.null(m)){
    message("Getting from local cache")
    return(m)
  }
  mat <- x$get()
  if(det(mat)!=0){
    inv <- solve(mat, ...)
    x$setInv(inv)
    return(inv)
  } else{
    message( "Matrix cant be inverted")
  }
}