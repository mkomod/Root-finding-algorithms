# 
# Implementation of the Bisection root finding algorithm in R
# Note: There are restrictive use cases as the function must be:
# - monotonic
# - continuous
#

bisection <- function(func, x.a, x.b, tol = 1e-05, iters = 10000) {
    #  Executes the bisection root finding algorithm
    # 
    # Args:
    #   func: (function) A function whose roots are to be determined
    #   x.a: (float) Endpoint value, i.e. lies before the root
    #   x.b: (float) Endpoint value, i.e. lies after the root
    #	[tol]: (float) The tolerance, determines when to stop running the algorithm.
    #	[iters]: (int) The maxiumum number of iterations to run before exitting
    #
    if ( ! (x.a < x.b) ) {
	stop("Invalid input provided x.a must be less than x.b" )
    } 
    if ( ! ( (func(x.a) < 0 && func(x.b) > 0) || (func(x.a) > 0 && func(x.b) < 0) ) ) {
	stop("The values of x.a and x.b provided do not lie between the root")
    }
   
    iter.vals = c()

    for (i in 1:iters) {
	
	x.c <- (x.a + x.b) / 2
	f.c <- func(x.c)

	iter.vals <- c(iter.vals, x.c)

	if ( f.c == 0 || abs(x.a - x.b) < tol) { 
	    # return(list(x.c = x.c, iter.vals = iter.vals))
	    return(x.c)
	}

	if (sign(func(x.a)) == sign(f.c)) {
	    x.a <- x.c
	} else {
	    x.b <- x.c
	}

    }

    if (i == iters) print("Maximum number of iterations exceeded")
}
