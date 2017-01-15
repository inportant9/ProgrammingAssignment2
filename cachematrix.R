# This assignment is aiming to create a function cache inverse of a matrix 

# The first function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# Yes = get the result and skips the computation. 
# No = compute the inverse, sets the value in the cache using setinverse function. 

# This function assumes that the matrix is always invertible. 
cacheSolve <- function(x, ...) { 
        inv <- x$getinverse() 
        if(!is.null(inv)) { 
                # get cached data given inverse matrix already exist 
                return(inv) 
        } 
        data <- x$get() 
        inv <- solve(data) 
        x$setinverse(inv) 
        inv 
} 

# Result Output 

> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) 
> my_matrix$get() 
     [,1] [,2] 
[1,]    1    3 
[2,]    2    4 
> my_matrix$getinverse() 
NULL 
> cacheSolve(my_matrix) 
     [,1] [,2] 
[1,]   -2  1.5 
[2,]    1 -0.5 
> my_matrix$getinverse() 
     [,1] [,2] 
[1,]   -2  1.5 
[2,]    1 -0.5 
> my_matrix$set(matrix(c(2,7,1,4),2,2)) 
> my_matrix$get() 
     [,1] [,2] 
[1,]    2    1 
[2,]    7    4 
> my_matrix$getinverse() 
NULL 
> cacheSolve(my_matrix) 
     [,1] [,2] 
[1,]    4   -1 
[2,]   -7    2
