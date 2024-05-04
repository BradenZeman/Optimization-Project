﻿# Numerical Optimization Techniques

What is optimization? Optimization is the mathematical process by which a function or value of interest is maximized or minimized (i.e. the search for a maxima or minima, collectively known as an optima). Why is optimization important? Optimization is used in processes such as least squares estimation, maximum likelihood estimation, statistical model selection criteria, or even to find critical thresholds and/or equilibria in applied mathematical models. It's safe to say that optimization is a multidisciplinary tool that is helpful for mathematicians, statisticians, modelers, and researchers alike.

Sometimes in the process of finding an optima, we can do so analytically and without much difficulty. However, a great deal of functions in mathematics cannot be solved analytically and thus require numerical techniques to find a solution. This is where computers can provide a great deal of aid in the iterative endeavor of numerical estimation. The following R package provides several techniques for univariate and multivariate numerical estimation. As the name implies, univariate refers to the optimization of differentiable functions with a single variable, while multivariate refers to optimization of a differentiable function with multiple variables. There is also combinatorial optimization which attempts to find optima within a discrete domain, but no numerical strategies have been provided at this time.

## Bisection Method

A univariate, iterative root-finding method. We define an upper and lower domain boundary and then calculate the midpoint. We then replace the upper or lower boundary with the midpoint according to a selection criteria. The midpoint is then recalculated. This process is repeated until the root is found or a stopping condition is met.

bisection(f, a, b, n=100, sigma = 0.0001, graph = FALSE)

f = Function given as an expression

a = Lower domain boundary of the region

b = Upper domain boundary of the region

n = Number of iterations to compute; Default is 100

sigma = Decimal place accuracy - stopping condition; Default is 0.0001

graph = If TRUE, two graphs will be returned; Otherwise, not used

## Secant Method

A univariate, iterative root-finding method. We define two initial starting points and then calculate the secant line between them. Afterwards, we find the x-value at which the secant line is equal to zero. We then calculate a new secant line between this new point and the previous domain point. This process is repeated until the root is found or a stopping condition is met.

secant(f, a, b, n=100, sigma = 0.0001, graph = FALSE)

f = Function given as an expression

a = First domain value required for secant line approximation

b = Second domain value required for secant line approximation

n = Number of iterations to compute; Default is 100

sigma = Decimal place accuracy - stopping condition; Default is 0.0001

graph = If TRUE, a graph will be returned; Otherwise, not used

## Newton's Method (Univariate + Multivariate)

A univariate, iterative root-finding method and a multivariate local linearization technique. In the univariate case, we define an initial starting point and calculate its tangent line. We then find the x-value at which the tangent line is equal to zero. Next, we calculate the tangent line at this new x-value. This process is repeated until the root is found or a stopping condition is met. In the multivariate case, we iterate according to a quadratic expansion of the Taylor series for the function.

Univariate:

newton(f, a, n=100, sigma = 0.0001, graph = FALSE)

f = Function given as an expression

a = Initial starting domain value

n = Number of iterations to compute; Default is 100

sigma = Decimal place accuracy - stopping condition; Default is 0.0001

graph = If TRUE, a graph will be returned; Otherwise, not used

Multivariate

newton2(f, a, b, n = 100, sigma = 0.0001, graph = FALSE)

f = Function given as an expression

a = Initial starting x value

b = Initial starting y value

n = Number of iterations to compute; Default is 100

sigma = Decimal place accuracy - stopping condition; Default is 0.0001

graph = If TRUE, two graphs will be returned; Otherwise, not used

# How to Install Package


