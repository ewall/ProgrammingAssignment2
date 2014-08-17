## Since matrix inversion can be a lengthy computation, these functions are speedier
## if you end up needing to calculate the same inverstion again, because they cache
## the result from the earlier calculation. The code is merely tweaked from the
## instructor's example of a similar caching closure.


## Given a matrix, makeCacheMatrix() creates a speical matrix object which also
## stores its inverse after the first time you call cacheSolve() on it. (Thanks
## to the magic of lexical scoping this function acts as a closure.) The function
## returns a list object with with getters & setters for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y # remember the matrix
        inv <<- NULL # erase any previously remembered inverse
    }
    get <- function() x

    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv

    # return a list with getter and setter methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Given an invertible matrix 'x' in the form of a makeCacheMatrix() object,
## cacheSolve() will return its inverse. If it has been called before for this
## same matrix, then it will return very quickly because it retrieves the cached
## value instead of calculating it again.

cacheSolve <- function(x, ...) {
    # check if x has a cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        # yup, we got a cached one, so simply return it
        message("(Fetching cached data)")
        return(inv)
    }
    # nope, we haven't solved this yet, so solve it now
    data <- x$get()
    inv <- solve(data, ...)
    # ...and cache the result in x
    x$setinverse(inv)
    inv
}


## testCacheSolve() tests the makeCacheMatrix() and cacheSolve() functions by
## using a sample matrix of random numbers, solving it for the inverse while
## timing the calculation, then solving yet again and timing to confirm that the
## cached value was returned more quickly.

testCacheSolve <- function(n=2000) {
    print("Testing the cached matrix inverse functions...")

    # seed random off current time
    t <- as.numeric(Sys.time())
    set.seed((t - floor(t)) * 1e8)

    # generate matrix of randoms
    vec <- runif(n*n,1,10) # random uniform distribution, low chance of degenerate
    mtx <- matrix(vec,nrow=n,ncol=n)

    # make it cacheable
    mcached <- makeCacheMatrix(mtx)

    # solve for inverse while timing
    time1 <- system.time( answer1 <- cacheSolve(mcached) )
    print(paste("Initial call:", round(time1[[3]],4), "seconds."))

    # solve again while timing
    time2 <- system.time( answer2 <- cacheSolve(mcached) )
    print(paste("Second call:", round(time2[[3]],4), "seconds."))

    # test if answer really is the inverse (matrix * inverse = identity)
    confirm <- identical( round(mtx %*% answer1, 1), diag(n) )
    if (confirm) {
        message("SUCCESS: Response is the inverse matrix.")
    } else {
        message("FAIL: Response doesn't fit the inverse!")
    }

    # ensure same answer given both times
    if (identical(answer1, answer2)) {
        message("SUCCESS: Same answer given each time.")
    } else {
        message("FAIL: Answers were not equal!")
    }

    # calculate and display time difference
    difference <- time1 - time2
    if (difference[3] > 0) {
        message("SUCCESS: Cached response was ", round(difference[3],4), " seconds faster.")
    } else {
        message("FAIL: Cached reponse was not faster!")
    }

    invisible(NULL)
}