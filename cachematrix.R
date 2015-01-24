## Put comments here that give an overall description of what your
## functions do

## This function return list of four method which save states of the object

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv_mat) m_inverse <<- inv_mat
        getinv <- function() m_inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Retrurn inverse of Matrix x, if already calculate return the cached inverse
## otherwise calculate it and cached it.

cacheSolve <- function(x, ...) {
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data_matrix <- x$get()
        m_inv <- solve(data_matrix, ...)
        x$setinv(m_inv)
        m_inv
}
