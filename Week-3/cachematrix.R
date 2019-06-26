## This code includes two functions, makeCacheMatrix() and cacheSolve(). The first function, makeCacheMatrix(), allows users to 
## initialize the matrix whose inverse they want to compute. Additionally, the function also allows users to set a new matrix, retrieve 
## the value of the matrix, set the value of an inverse, and retrieve the value of the inverse. The second function, cacheSolve(), 
## returns the inverse of a matrix by retrieving the inverse from the cache if already existing, or by calculating it if not existing. 

## The first function, makeCacheMatrix(), builds a set of functions and returns the functions within a list. For example, the following 
## code "myMatrix <- makeCacheMatrix()" creates a list object called "myMatrix" which contains four functions including "set(), 
## get(), setInverse(), and getInverse()". 
## 1. The set() function allows users to input a matrix if not initialized or replace the original matrix by creating a new one.
## 2. The get() function retrieves the matrix users set.
## 3. The setInverse() function assigns the inverse of a matrix to a variable called "i". More importantly, this function can be called 
## by the next function cacheSolve() to store the inverse after computation. 
## 4. The getInverse() function retrieves the inverse of the matrix.

## For example, let's create a matrix called "myMatrix" by calling the makeCacheMatrix() function using the below code.
## myMatrix <- makeCacheMatrix()
## Users are able to initialize the matrix when calling the function, for example with the following code. 
## myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) 
## If no initialization is performed, users then need to set the matrix by calling the set() function. For example,
## myMatrix$set(matrix(1:4, 2, 2))
## Once the matrix set, users can check the matrix by calling the get() function with the code myMatrix$get().

makeCacheMatrix <- function(x = matrix()) {
    ## Set i to NULL when initializing a new matrix
    i <- NULL
    
    ## Use the argument y to differentiate from x. Both x and y are matrice.  
    set <- function(y){
        ## Replace the old matrix x with the new matrix y. If no matrix is input by users when the main function is called, x will be an
        ## empty matrix because of the default value of x is initialized via matrix().
        x <<- y
        
        ## Set i to NULL because a new matrix has been created, and the inverse has not been computed yet.
        i <<- NULL
        
        ## The assignment operator "<<-" instead of "<-" is used in the set() function because x and i are not defined in the set() 
        ##function, and "<<-" can be used to assign values to variables that are not in the current environment.
    }
    ## Retrieve the matrix x.
    get <- function() x
    
    ## Assign the inverse to the variable i. Be noted that the argument "inverse" here is going to be computed when calling the 
    ## subsequent cacheSolve() function.
    setInverse <- function(inverse) i <<- inverse
    
    ## Retrieve the inverse i.
    getInverse <- function() i
    
    ## Store the above four functions in a list, and return this list when the main function ends. That being said, by calling
    ## makeCacheMatrix(), a list object is created, and this list object consists four elements with each element corresponding to a 
    ## function whose name is exactly the same to the element's. Thus, this allows users to access those fuctions using $ + the
    ## functino name. For example, object$get() calls the get() function.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function, cacheSolve(), returns a matrix that is the inverse of "x". If the inverse has already been stored in the object x 
##(i is the variable which keeps the inverse), the function returns the inverse directly. If not, the function retrieves the matrix that
## requires inverse computation and then computes the inverse by calling the built-in function solve(). The inverse will be next assigned
## to the variable i and stored to the object x by calling the setInverse() function. 

## x is the list object we created previously.
cacheSolve <- function(x, ...) {
    ## Retrieve the inverse by calling the getInverse() function.
    i <- x$getInverse()
    
    ## If i does not equal NULL, which means the inverse has already been stored in the object, the function returns the inverse directly.
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    ## If i equals NULL, the below code will be executed. First, the function retrieves the matrix by calling the get() function and 
    ## assigns the matrix to the variable dat. Next, the solve function is called to compute the inverse of the matrix, and the inverse 
    ## will be assigned to the variable i. Be noted that, the i here is defined within the cacheSolve() function, so we have to store its
    ## value into the object by calling the setInverse() function. Finally, the function return the inverse i.
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
