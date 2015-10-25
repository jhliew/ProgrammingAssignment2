## In overall, makeCacheMatrix() is function that allow user to set the matrix to
## global variable x. After user finished setting the matrix to global variable x, 
## they can then compute the inverse matrix of x by using cachaSolve() function.

## if the supplied matrix is computable i.e it is not singular, then function continue to
## calculate the inverse matrix and cache the inversed matrix for future usage. 
## Else, error message will be prompt where user is ask to input another 
## matrix with different content.

## Usage of functions and demo steps:
## 1. Create square matrix
##    mm = matrix(c(1:4), 2, 2)

## 2. Call makeCacheMatrix and store it
##    cm = makeCacheMatrix()

## 3. Set mm to global variable x
##    cm$set(mm)

## 4. Run cacheSolve function to get the inversed matrix of x
##    cacheSolve(cm)


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  #Accessor and Mutator for globle variables x and inverseMatrix
  set <- function(matrixData)
  {
    x <<- matrixData
    inverseMatrix <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInverseMatrix <- function(inverseMatrixData)
  {
    inverseMatrix <<- inverseMatrixData
  }
  
  getInverseMatrix <- function()
  {
    inverseMatrix
  }
  
  # Return a list containing the memory location of each component.
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


cacheSolve <- function(x, ...) {
  
  # Check if inversed matrix of x is already cached
  inversedMatrix <- x$getInverseMatrix()
  if(!is.null(inversedMatrix))
  {
    message("Getting cached inverse matrix of x")
    return(inversedMatrix)
  }
  
  # Else, get the matrix and compute its inverse
  data <- x$get()
  inversedMatrix <- try(solve(data), TRUE)
  
  # If result return from try() is a matrix, cache and print the inversed matrix
  if(is.matrix(inversedMatrix))
  {
    x$setInverseMatrix(inversedMatrix)
    message("Inverse Matrix of x is calculated and cached")
    inversedMatrix
  }
  
  # Else, prompt user to supply another square matrix with different content
  else
  {
    message("Matrix supplied is exactly singular. Please supply another square matrix.")
  }
  
}
