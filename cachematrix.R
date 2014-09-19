
	#The makeCacheMatrix returns a list of functions we can use to get and
	#set the input matrix. It also has appropriately named 
	#functions to solve for the input's inverse
	#Input : Accepts a matrix and stores it in x$get
	#Output : List containing usable functions for getting and setting X
	
	makeCacheMatrix <- function(x = matrix()) {
	# Accepts a matrix and stores it in a variable named x
	 
	  inverseX <- NULL 
	  #Stores the inverse of x. Initializes as NULL
	  
	  
	  # Setter function
	  set <- function(y) { 
		   x <<- y
		   inverseX <<- NULL
	  }
	  #Getter function
	  get <- function() {
			x
	  }
	  # Sets/Calculates the inverse of X
	 setinverse <- function(m) inverseX <<- m
	 #Getter function for the inverse of X
	 getinverse <- function() inverseX
	 #Returns a list containing all the required functions
	  list(set = set, 
		   get = get,
		   setinverse = setinverse,
		   getinverse = getinverse
		   )
	  

	 
	}

	#The cachesolve function returns the inverse of an input matrix.
	#Input :  A matrix
	#Output : Inverse of given matrix


	  cachesolve <- function(x, ...) {
		  inv <- x$getinverse()
		  if(!is.null(inv)) 
		  { 
		    # If the inverse of X already exists then it is NOT null. Thus 
			# it is in the cached
			# Thus all we do is take the already existing inverse and
			# return it back to whoever called it.
				message ("Getting the cached inverse of X")
				return (inv)
		  }
		   # If the inverse does not already exist then we find the 
		   # inverse of X using the solve method
		   #  After inverting the matrix we set inverseX as the 
		   # inverse of X by calling the setter function.
		  data <- x$get() 
		  inv <- solve(x)
		  x$setinverse(inv)
		  inv
	}
 