## The first function, makeCacheMatrix, creates a special object from an
## invertable square matrix. This object is a list of four functions, plus
## any cached values stored in teh makeCacheMatrix environment.
##
## The second function either perfoms the solve function on the matrix, or
## simply returns the cached value if the solve function has already been 
## performed and the value of the matrix has not changed.

## This function will create a special object which is a list of four functions
## plus the cached values of a matrix and the inverse of the matrix after they are
## defined within the function environment

makeCacheMatrix <- function(x = matrix()) {
		#Assign the default value of inv to NULL
        inv <- NULL
        #This function sets the value of the matrix in the makeCacheMatrix function
        #Re-sets the value of inv to NULL if it has changed
        set <- function(y){
                #Assign the value of the matrix (x) to the value of the 
                #new arguement (y) in the makeCacheMatrix environment
                x <<- y
                #Re-set the value of inv to null in the makeCacheMatrix environment
                inv <<- NULL
        }
        #This function gets the value of the matrix
        get <- function() x
        #This function calculates the value of the inversion, and
        #assigns that value to inv in the makeCacheMatrix environment
        setInverse <- function() inv <<- solve(x)
        #This function gets the value of the inv
        getInverse <- function() inv
        #The final output is a list of functions, as well as the objects defined
        #in the makeCacheMatrix environmentk
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function will either calculate the inverse of the matrix, if it has 
## been defined, or will return the cached value of the inverse of the matrix,
## if that has also been defined

cacheSolve <- function(x, ...) {
        #Calls the getInverse function and sets inv to the value of the inversion
        inv <- x$getInverse()
        #Checks to see if the value of the inversion, which was defined in the 
        #makeCacheMatrix environment and retrieved by the getInversion function,
        #is null; if not, returns a message and the value of inv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #If there is no cached value for inv in the makeCacheMatrix function and
        #the value of inv is NULL, the rest of the function calculates the value
        #Calls the setInverse function to calculate the inverse and assign the
        #value to inv
        inv <- x$setInverse()
        #returns the value of inv
        inv
}
