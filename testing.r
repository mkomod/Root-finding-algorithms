
library("testthat")

source("functional_iteration.r")
source("bisection.r")
source("newton_raphson.r")


###############################################################################
#
# Functional Iteration
#
###############################################################################

test_that("A fixed point is found for f(x) = x ^ 2", {
    values <- functional.iteration(function(x) x^2, 0.5)
    expect_equal(round(values$x.new, digits=8), 0)
})

test_that("An error is raised when determining the fixed point of f(x) = e^x", {
    expect_error(do.call(functional.iteration(function(x) e^x, 0.5)))
})


###############################################################################
#
# Bisection
#
###############################################################################

test_that("Root found for monotonic functions", {
    expect_equal(round(bisection(function(x) 2 - x, 0, 5), digits=4), 2)
    expect_equal(round(bisection(function(x) x ^ 2 - 4, 0, 5), digits=4), 2)
    expect_equal(round(bisection(function(x) exp(x) - 5, 0 , 5), digits=4), 1.6094)
})

test_that("Bisection fails for non monotonic function", {
    expect_error(do.call(bisection(function(x) x^2, 0, 5)))
})

test_that("Bisection fails for interval not between root", {
    expect_error(do.call(bisection(function(x) 2 - x, 5, 7)))
})


###############################################################################
#
# Newton Raphson
#
###############################################################################

test_that("Root found for functions", {
    vals <- newton.raphson(function(x) 2 - x, 0)
    expect_equal(round(vals$root, digits=4), 2)

    vals <- newton.raphson(function(x) (x ^ 2) - 4, 1)
    expect_equal(round(vals$root, digits=4), 2)

    vals <- newton.raphson(function(x) exp(x) - 5, 1)
    expect_equal(round(vals$root, digits=4), 1.6094)

    vals <- newton.raphson(function(x) x ^ 2, 4)
    expect_equal(round(vals$root, digits=4), 0)
})

test_that("Newton Raphson fails for divergent funciton", {
    expect_error(do.call(newton.raphson(function(x) exp(x), 5)))
})

