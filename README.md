# Numerical Optimization Techniques

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

Multivariate:

newton2(f, a, b, n = 100, sigma = 0.0001, graph = FALSE)

f = Function given as an expression

a = Initial starting x value

b = Initial starting y value

n = Number of iterations to compute; Default is 100

sigma = Decimal place accuracy - stopping condition; Default is 0.0001

graph = If TRUE, two graphs will be returned; Otherwise, not used

# How to Install Package

Prerequisites: 

1) Must have R and Rstudio installed on your computer
2) Make sure to download the package (plotly) using install.packages(plotly) in an R script

Optimization Package Installation:

1) Click on the green "Code" button above
2) Click on the "Download Zip" button from the dropdown menu
3) When everything is done downloading, open up the project in Rstudio
4) Navigate to the top-left of your screen and click "Build"
5) Click "Install Package" from the dropdown menu
6) If your R session is reloaded and library(Optimization) is printed, then the package is installed
7) Navigate to the top-left of your screen and click "File"
8) Click "Close Project" from the dropdown menu
9) Now anytime you open up an R script, you only need to type library(Optimization) for the package to be loaded into the session
10) From here, you can experiment with the different numerical optimization techniques; to see what they do, refer to the help page

# How to access the Help Documentation

To access the main help file:

help(Optimization)

To access the bisection method help file:

help(bisection)

To access the secant method help file:

help(secant)

To access the univariate Newton's method help file:

help(newton)

To access the multivariate Newton's method help file:

help(newton2)

# Usage Examples

First, we define a function of interest as an expression. 

f = expression(-x^2+4*x)

Below is the bisection method function. Notice the inputs: 1) a function, 2) lower boundary, 3) upper boundary.

bisection(f, 0, 4, graph = T)

Now we have the secant method function. Notice the inputs: 1) a function, 2) lower point, 4) upper point.

secant(f, 0, 1, graph = T)

Now we have Newton's method. Notice the inputs: 1) a function, 2) a starting point.

newton(f, 1, graph = T)

For multivariate Newton's method, we should define a multivariate function of interest as an expression.

f = expression(sqrt(4-x^2-y^2))

Below is Newton's method for 3D space. Notice the inputs: 1) a function, 2) a starting x input, 3) a starting y input.

newton2(f, 0.5, 1, graph = T)
