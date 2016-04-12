## Put comments here that give an overall description of what your
## functions do

#The first function, makeCacheMatrix creates a special "matrix" object, which contains a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the solve
#get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #creation of a undefined object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# The following function calculates the inverse of the special "matrix" object created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. We assume that the matrix supplied is always invertible.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#Test
# > matrice<- matrix(c(1,3,2,4),nrow =2, ncol=2)
# > m = makeCacheMatrix(matrice)
# > m$get()
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4

# In the first test the value is not in cache so the function has to calculates it
# > cacheSolve(m)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# In the second test the value is in the cache 
# > cacheSolve(m)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

