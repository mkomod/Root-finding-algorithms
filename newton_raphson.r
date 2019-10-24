# 
# Implementation of the Newton Raphson method for finding roots
# Note: the implementation relies on the use of the following external packages
#  - numDeriv
#



# install.packages("numDeriv")
require("numDeriv")

newton.raphson <- function(func, x_0, tol = 1e-5, iters = 1000) {
    # Performs newton raphson root finding algorithm and finds a root
    #  of a given function
    #
    # Args:
    #	func: (funciton) A function whose roots are to be determined.
    #	x_0: (float) The starting value / guess.
    #	[tol]: (float) The tolerenace / change limit at which the new value
    #		is taken as the root.
    #	[iters]: (int) The number of iterations to perform before exiting
    #
    # Returns:
    #	(float) The root of a given function
    #
    # Warnings:
    #	If no root is found a warning is raised and the script terminated
    #
    iter.list = c(x_0)

    for (i in 1:iters) {

	df.dx <- grad(func, x_0)
	x_1 <- x_0 - ( func(x_0) /  df.dx )
	iter.list <- c(iter.list, x_1)

	tryCatch({

	    res <- list(root = x_1, x_0 = x_0, df.dx = df.dx, iter.list = iter.list, iters = i)
		
	    if ( is.nan(x_1) ) {

		stop(paste("No root found in", i, "iterations"))

	    } else if ( is.infinite(x_1) ) {

		stop(paste("No root found in", i, "iterations, divergance to infinity"))

	    } else if (abs(x_1 - x_0) < tol) {

		return(res)

	    }

	}, error = function(err) {

	    print("An error occured, the function provided may not have a root")
	    print(res)
	    stop(err)

	})

	x_0 <- x_1
    }

    if ( i == iters ) warning("Number of iterations exceeded")

}
