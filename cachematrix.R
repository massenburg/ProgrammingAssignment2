## Put comments here that give an overall description of what your
## makeCacheMatrix2 takes a matrix, creates a set function to store the matrix value
## "  " creates a get function to retrieve matrix value
## "  " creates a setinv and getinv function to store and retrieve the inverse value
## "  " also relys on the cacheSolve function to calcuate inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  #check whether there is already an inverse calculated, if not then set to NULL
  if(!exists('inv')) {
    inv <- NULL
  }  
  #set value of x to be input matrix
  set <- function(y) {
    x <<- y
  }
  #function to retrive input matrix value
  get <- function() x
  
  #function to store and get inverse matrix values
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  #calculate inverse by calling cacheSolve and object allows cacheSolve to retrieve matrix & inverse values
  inv <- cacheSolve(list(get=get,inv = inv, getinv=getinv))
  #output or special list
  list(set = set, get = get, setinv=setinv, getinv=getinv)
  
}

## cacheSolve checks if inverse is stored and whether stored inverse the true inverse of the current matrix
## if there is no stored inverse or if it isn't the inverse, then cacheSolve will calc the inverse using solve

cacheSolve <- function(x, ...) {
  #get matrix & inverse values from passed input object
  mat <- x$get()
  inv <- x$getinv()

  #check if inverse is null, if so then calculate inverse and return it
    if(is.null(inv)) {
    inv <- solve(mat)
    return(inv) 
  } else {
  #if not null, then multiply matrix by inverse
    eye <- round(inv %*% mat)
    #check if product of matrix & inverse is identity matrix
    if(identical(eye, diag(nrow(mat)))) {
    #if product is identity matrix, then passed inverse is true inverse and return it
      return(inv)
    } else {
    #if product isn't identity matrix, then recalculate inverse and return it
      inv <- solve(mat)
      return(inv)  
    }
  }
}
