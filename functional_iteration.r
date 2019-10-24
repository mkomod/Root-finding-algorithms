#
# Implementation of the functional iteration algorithm in R
# Functional iteration is used to find a fixed point. 
# For instance to determine when x = g, for some f(x)= g
# Functinal iteration can be used to find roots with the appropriate f(x)


functional.iteration <- function(func, x_0, tol = 1e-05, iters = 10000) {
    # Performs functional iteration
    #
    # Args:
    #   func: (function) A funciton to iterate
    #   x_0: (float) the starting value for the algorithm
    #   tol: (float) the tolerance of the algorithm
    #	iters: (int) the number of iterations to perform before exiting
    # 
    # Returns:
    #   (list) A list containing the final and penultimate vlaues, the iteration values and
    #   the number of iterations
    #
    # Warnings:
    #	If no root is found then a warning is raised and the funciton is terminated
    #
    x.old <- x_0
    x.iters <- c(x.old)

    for (i in 1:iters) {

	x.new <- func(x.old)
	x.iters <- c(x.iters, x.new)

	tryCatch({

	    if ( is.nan(x.new) ) {

		stop(paste("No fixed point found in", i, "iterations"))

	    }  else if ( is.infinite(x.new) ) {

		stop(paste("Function has divered to infinity in", i, "iterations" ))

	    } else if (abs(x.new - x.old) < tol) {

		return(list(x.old = x.old, x.new = x.new, x.iters = x.iters, iters = i))

	    }

	}, error = function(err) {

	    print("An error occured, there may be an issue with the function provided")
	    print(list(x.old = x.old, x.new = x.new, x.iters = x.iters, iters = i))
	    stop(err)

	})

	x.old <- x.new
    }

    if ( i == iters ) warning("Number of iterations exceeded")

}

