## Both these functions together help in caching the inverse of 
## a matrix

## This function is a function that gives the list - set,get,setinv, getinv
## as an output. This will be assigned to a variable and will contain the 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse<- NULL # initialising an inverse varible
    
    # function to feed the matrix to be operated on
    set <- function (y) {
        x<<-y
        inverse <<- NULL
    } 
    get <- function () x # a function used to retrieve the matrix
    ## the function "ginv" is from the MASS package to calculate inverse of a matrix
    setinv <- function (inv) inverse<<- inv # set the inverse of the matrix
    getinv <- function () inverse  # get the inverse of the matrix
    list (set=set, get=get, setinv=setinv, getinv=getinv) # output functions
}


## This function will operate on a variable that is assigned the 
## "makeCacheMatrix" function. This will return the inverse if already
## computed and if not, t will compute inverse and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #getting inverse if already computed
    inv <- x$getinv() 
    if(!is.null(inv)){
        message("obtaining the cached data")
        return(inv)
    }
    #calculating inverse if not already computed
    matrix <- x$get()
    inv <- ginv(matrix)
    x$setinv(inv)
    inv
}
