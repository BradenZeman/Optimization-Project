\name{Optimization}
\alias{Optimization}

\title{Optimization R Package}

\description{What is optimization? Optimization is the mathematical process by which a function or value of interest is maximized or minimized (i.e. the search for a maxima or minima, collectively known as an optima). Why is optimization important? Optimization is used in processes such as least squares estimation, maximum likelihood estimation, statistical model selection criteria, or even to find critical thresholds and/or equilibria in applied mathematical models. It's safe to safe that optimization is a multidisciplinary tool that is helpful for mathematicians, statisticians, modelers, and researchers alike.

Sometimes in the process of finding an optima, we can do so analytically and without much difficulty. However, a create deal of functions in mathematics cannot be solved analytically and thus require numerical techniques to find a solution. This is where computers can provide a great deal of aid in the iterative endeavor of numerical estimation. The following R package provides several techniques for univariate and multivariate numerical estimation. As the name implies, univariate refers to optimization of a differentiable functions with a single variable, while multivariate refers to optimization of a differentiable function with multiple variables. There is also combinatorial optimization which attempts to find optima within a discrete domain, but no numerical strategies have been provided at this time.}

\details{Bisection Method (\link{bisection}): A univariate, iterative root-finding method. We define an upper and lower domain boundary and then calculate the midpoint. We then replace the upper or lower boundary with the midpoint according to a selection criteria. The midpoint is then recalculated. This process is repeated until the root is found or a stopping condition is met.

Secant Method (\link{secant}): A univariate, iterative root-finding method. We define two initial starting points and then calculate the secant line between them. Afterwards, we find the x-value at which the secant line is equal to zero. We then calculate a new secant line between this new point and the previous domain point. This process is repeated until the root is found or a stopping condition is met.

Newton's Method (\link{newton}, \link{newton2}): A univariate, iterative root-finding method and a multivariate local linearization technique. In the univariate case, we define an initial starting point and calculate its tangent line. We then find the x-value at which the tangent line is equal to zero. Next, we calculate the tangent line at this new x-value. This process is repeated until the root is found or a stopping condition is met. In the multivariate case, we iterate according to a quadratic expansion of the Taylor series for the function.
}

\author{Braden Zeman}

\references{[1] Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.

[2] "Virtual Library of Simulation Experiments:" Optimization Test Functions and Datasets, \href{https://www.sfu.ca/~ssurjano/optimization.html}{https://www.sfu.ca/~ssurjano/optimization.html}}

\examples{
# Below are examples of all univariate and multivariate techniques mentioned
# above (i.e. an example of each function included in the package). We will
# start with the univariate methods.

# First we define a function of interest as an expression.
f = expression(-x^2+4*x)

# Below is the bisection method function. Notice the inputs:
# 1) a function, 2) lower boundary, 3) upper boundary.
bisection(f, 0, 4)

# Now we have the secant method function. Notice the inputs:
# 1) a function, 2) lower point, 4) upper point.
secant(f, 0, 1)

# Lastly we have Newton's method. Notice the inputs:
# 1) a function, 2) a starting point.
newton(f, 1)

# Now we move onto the multivariate methods.

# First we will define a function of interest as an expression.
f = expression(sqrt(4-x^2-y^2))

# Below is Newton's method for a 3D space. Notice the inputs:
# 1) a function, 2) a starting x input, 3) a starting y input.
newton2(f, 0.5, 1)

# Before proceeding, it would be best to be transparent and
# inform that numerical optimization methods search for any
# nearby optima. Thus, the methods within this package
# will find local optima even if the user wants global.

# Consider the following examples:

# Gramacy & Lee Function
f = expression((sin(10*pi*x)/(2*x)+(x-1)^4))
bisection(f, 0.5, 2.5, graph = T)
secant(f, 0.5, 2.5, graph = T)
newton(f, 1.5, graph = T)

# Drop-Wave function
# Global Min at (0,0,-1)
f = expression(-(1+cos(12*sqrt(x^2+y^2)))/(0.5*(x^2+y^2)+2))
newton2(f, 1, 1, graph = T)
newton2(f, 0.5, 0.5, graph = T)
newton2(f, 0.05, 0.05, graph = T)

# Levy Function N.13
# Global Min at (1,1,0)
f = expression(sin(3*pi*x)^2+(x-1)^2*(1+(sin(3*pi*y)^2))+(y-1)^2*(1+(sin(2*pi*y)^2)))
newton2(f, 1.2, 1.2, graph = T)
newton2(f, 1.01, 1.01, graph = T)

# Schaffer Function N.2
# Global Min at (0,0,0)
f = expression(0.5+(sin(x^2-y^2)^2-0.5)/(1+0.001*(x^2+y^2))^2)
newton2(f, 0.5, 0.5, graph = T)
newton2(f, -1, -2, graph = T)

}
