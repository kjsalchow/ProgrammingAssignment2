## Put comments here that give an overall description of what your
## functions do

## instantiates an object of type makeCacheMatrix consisting of matrix and,
## the inverse of the matrix in cache.
makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    set <- function(makeCache) {
        x <<- makeCache
        cached <<- NULL
    }
    get <- function() x
    setCache <- function(solved) cached <<- solved
    getCache <- function() cached
    list(set=set, get=get, setCache=setCache, getCache=getCache)
}


## Returns the inversion of the supplied matrix either from cache, or by,
## calculating it, putting it in cache and then returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cached <- x$getCache()
    if(!is.null(cached)){
        message("getting cached data")
        return(cached)
    }
    data <-x$get()
    cached <- solve(data, ...)
    x$setCache(cached)
    cached
}
