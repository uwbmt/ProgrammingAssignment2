## This file contains three functions. 
## makeCacheMatrix(x) creates a list object containing 4 functions to set and return a matrix 
## as well as its inverse.
## cacheSolve() allows getting the inverse of a matrix. 
## Either by computing or by accessing from the cache.
## test(z) uses a matrix z to test the two functions described above.


## This function creates an object that contains 4 function.
## set(y) allows assigning values y to the object. In this case a matrix.
## get() allows returning these values.
## setinverse(inverse) allows setting the inverse. Note: no inverse is computed at this point. 
## i.e. setinverse(inverse) needs the actual inverse to assign it to the object.
## getinverse() returns the inverse if it has been assigned before and null otherwise.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function has as input argument of the return type from makeCacheMatrix, 
## i.e. a list object with 4 functions.  
## First the function calls getinverse() from the input object. 
## If something other then null is returned, i.e. an inverse is cached already, 
## a corresponding message is printed and the inverse matrix is returned.
## Note that this exits cacheSolve() so that no inverse is computed at potentially high cost.
## If getinverse() returnes null, the inverse needs to be computed with solve().
## Further, the newly found inverse is set in the input object so that it is cached from now on.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        return(i)
}


## The test function can be used to test the two above function.
## It first prints out the original matrix.
## Then cacheSolve is called two times. 
## The first time the cache is empty and the inverse is computed.
## The second time the inverse returned out of the cache without computing again. 

test <- function(x){
        message('This is the input matrix')
        print(x)
        message('Make Cache Matrix')
        A <- makeCacheMatrix(x)
        A$set(x)
        message('Test cacheSolve when nothing is in cache yet')
        print(cacheSolve(A))
        message('Test cacheSolve for second time, i.e. with inverse matrix in cache already')
        print(cacheSolve(A))
}
