\name{Optimization}
\alias{optimization}

\title{Optimization R Package}

\description{What is optimization? Optimization is the mathematical process by which a function or value of interest is maximized or minimized (i.e. the search for a maxima or minima, collectively known as an optima). Why is optimization important? Optimization is used in processes such as least squares estimation, maximum likelihood estimation, statistical model selection criteria, or even to find critical thresholds and/or equilibria in applied mathematical models. It's safe to safe that optimization is a multidisciplinary tool that is helpful for mathematicians, statisticians, modelers, and researchers alike.

Sometimes in the process of finding an optima, we can do so analytically and without much difficulty. However, a create deal of functions in mathematics cannot be solved analytically and thus require numerical techniques to find a solution. This is where computers can provide a great deal of aid in the iterative endeavor of numerical estimation. The following R package provides several techniques for univariate and multivariate numerical estimation. As the name implies, univariate refers to optimization of a differentiable functions with a single variable, while multivariate refers to optimization of a differentiable function with multiple variables. There is also combinatorial optimization which attempts to find optima within a discrete domain, but no numerical strategies have been provided at this time.}

\details{Bisection Method (\link{Bisection.Rd}): A univariate, iterative root-finding method. We define an upper and lower domain boundary and then calculate the midpoint. We then replace the upper or lower boundary with the midpoint according to a selection criteria. The midpoint is then recalculated. This process is repeated until the root is found or a stopping condition is met.

Secant Method (\link{Secant.Rd}): A univariate, iterative root-finding method. We define two initial starting points and then calculate the secant line between them. Afterwards, we find the x-value at which the secant line is equal to zero. We then calculate a new secant line between this new point and the previous domain point. This process is repeated until the root is found or a stopping condition is met.

Newton's Method (\link{Newton.Rd}, \link{Newton2.Rd}): A univariate, iterative root-finding method and a multivariate local linearization technique. In the univariate case, we define an initial starting point and calculate its tangent line. We then find the x-value at which the tangent line is equal to zero. Next, we calculate the tangent line at this new x-value. This process is repeated until the root is found or a stopping condition is met. In the multivariate case, we iterate according to a quadratic expansion of the Taylor series for the function.

Nelder-Mead Algorithm (\link{NelderMead.Rd}): A multivariate local linearization technique. First we compute a triangular boundary and rank the three vertices (Worst, Bad, Best). We then calculate the centroid on the "best" face of the triangle (i.e. the face that connects the vertices Bad and Best). Next, we engage in an iterative process of reflection, expansion, contraction, or shrinkage of the triangle according to selection critera. This process is repeated until the root is found or a stopping condition is met.}

\references{Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.}

\author{Braden Zeman}

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
}
