## This function creates a list of four "subfunctions":
## 1. set - caches the matrix passed to makeCacheMatrix or allows the entry of a different matrix to cache;
## 2. get - recalls the cached matrix;
## 3. setinverse - cahes the inverse of the cached matrix;
## 4. getinverse - recalls the cached inverse. 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the matrix (via the list) passed to makeCacheMatrix and:
## 1. checks if the inverse has already been computed, in whcich case it calls it from the cache;
## 2. otherwise it computes the inverse and returns it. 
 

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
