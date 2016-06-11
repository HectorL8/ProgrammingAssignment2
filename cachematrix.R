# The idea of the functions below is to make an auxiliary list of elements 
# to make the conditional statments necessary to either calculate the 
# inverse matrix or just return an already calculated inverse matrix. 

# To run these functions you must first pass the matrix to be inverted
# as an argument to the function makeCacheMatrix() and store it in a
# new variable. Then, you must run the function cacheSolve() with this 
# new variable.

# The next time you run cacheSolve() with the same variable the message
# 'getting cached data' will appear indicating that the inverse matrix 
# wasn't recalculated.

# 1st FUNCTION:

# This function creates a list where the 1st element is a function to
# set the matrix passed as an argument to the function makeCacheMatrix 
# to the variable 'x' in the environment of the function makeCacheMatrix 
# ('<<-' defines the variable in an outer environment); the 2nd element 
# is a function to get the variable 'x' (matrix); the 3rd element is a 
# function to set the inverse matrix (passed as an argument) to the 
# variable 'inv' in the environment of makeCacheMatrix (using '<<-'); and 
# the 4th element is a function to get the variable 'inv' (inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) inv <<- Inv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# 2nd FUNCTION:

# This function works similarly to the solve() function except for two things:
# it takes as an argument a list with the structure defined in the 1st FUNCTION 
# and it first tests to see if there is an already cached inverse of the 
# relevant matrix. If there is no cached inverse (NULL value) then the function 
# calculates the inverse matrix and returns this value, if there is an already 
# cached inverse matrix, then the function returns the cached value and a message 
# saying 'getting cached data'.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

# Additional comments:

# The variable 'inv' gets modified the first time you calculate the matrix 
# inverse. Nevertheless, as it is only defined inside the function, you cannot
# observe it in the Global Environment but you can call it with x$getInv().

# If you call x$getInv() without running cacheSolve() then it will appear as a
# NULL value; after running the function cacheSolve() then x$getInv() will return
# the inverse matrix.
