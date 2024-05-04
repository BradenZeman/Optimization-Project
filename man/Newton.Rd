
%% Name of file and alias to distinguish from other files
\name{Newton}
\alias{newton}

%% Title is given Newton's method
\title{Newton's Method (Univariate)}

%% Description of what the function does and the theory behind Newton's method
\description{A univariate, iterative root-finding method. We define an initial starting point and calculate its tangent line. We then find the x-value at which the tangent line is equal to zero. Next, we calculate the tangent line at this new x-value. This process is repeated until the root is found or a stopping condition is met.}

%% Identifies the function and parameters needed
\usage{newton(f, a, n=100, sigma = 0.0001, graph = FALSE)}

%% Description of arguments
\arguments{
  \item{f}{Function given as an expression.}
  \item{a}{Initial starting domain value.}
  \item{n}{Number of iterations to compute. Default is 100.}
  \item{sigma}{Decimal place accuracy - stopping condition. Default is 0.0001.}
  \item{graph}{If TRUE, a graph will be returned. Otherwise, not used.}
}

\details{The selection criteria at each iterative step:

\eqn{x^{(t+1)}=x^{(t)}-\frac{g'(x^{(t)})}{g''(x^{(t)})}}

This updating equation was derived by approximating the tangent line at the maxima critical value, \eqn{g'(x^*)}, using the linear Taylor series expansion:

\eqn{g'(x^*)=g'(x^{(t)})+(x^*-x^{(t)})g''(x^{(t)})}

If the critical value is the maxima, we expect the first derivative to be equal to zero.

\eqn{0=g'(x^{(t)})+(x^*-x^{(t)})g''(x^{(t)})}

Now we simply rearrange.

\eqn{-g'(x^{(t)})=(x^*-x^{(t)})g''(x^{(t)})}

\eqn{-\frac{g'(x^{(t)})}{g''(x^{(t)})}=(x^*-x^{(t)})}

\eqn{x^*=x^{(t)}-\frac{g'(x^{(t)})}{g''(x^{(t)})}}

Lastly, we rename \eqn{x^*} to \eqn{x^{(t+1)}}. Now we have the updating equation.

}

\value{If graph = FALSE, a list of values will be returned. In that list will be the Optima x-value, Optima y-value, the number of iterations needed to reach stopping condition (n), and the decimal level of accuracy (sigma).

If graph = TRUE, that same list of values will be returned with a graph. The graph plots the function and highlights the optima point.}

\references{[1] Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.

[2] "Virtual Library of Simulation Experiments:" Optimization Test Functions and Datasets, \href{https://www.sfu.ca/~ssurjano/optimization.html}{https://www.sfu.ca/~ssurjano/optimization.html}}

\author{Braden Zeman}

\note{See related optimization functions \link{bisection}, \link{secant}, \link{newton2}.}

\examples{
# The following examples will show how to run and utilize each of the
# arguments in the Newton function.

# First we must define a function of interest as an expression.
f = expression(log(x)/(1+x))

# We are required to input a function and a starting point in
# order to calculate the tangent line. Using the tangent line, we will
# incrementally approach the optima for the function.
newton(f, 3)

# If finding an optima is computationally intensive, we can specify
# the number of iterations we wish to compute. The default is 100,
# but that number may not be needed if stopping conditions are met.
newton(f, 3, n = 3)

# We can also specify the decimal accuracy we wish to achieve when
# finding an optima. The sigma value is directly tied to the stopping
# condition. Only the necessary number of iterations will be conducted
# until the sigma value is maintained across an iteration. Thus, the
# sigma input is prioritized over the iteration number input. The
# default sigma is four decimal places.
newton(f, 3, sigma = 0.00001)

# Note: for the faster-converging optimization methods, the true sigma
# value may be smaller than the returned sigma since the method
# gains accuracy very quickly across small iterations. Thus, treat
# sigma as a measure of "at least" this good.

# Lastly, we can chose to output a graph of Newton's method. The
# graph will plot the function and highlight the optima point.
newton(f, 3, graph = T)

# In the following section, an example will be given to outline what
# an "optima" actual is. It is occurs when the slope or derivative of
# a function is zero. This can occur as a maxima, minima, or other.
# Consider the following:
f = expression((x-2)^3+2)
newton(f, 0, graph = T)

# The optima point identified is neither a maximum nor a minimum.
# Instead it is the location when the slope of the function is zero.
# Keep this in mind as you optimize functions and input intervals.

# Consider another example of the importance of defining intervals:
f = expression(-x^4+x^3+x^2+3)
newton(f, 0.2, graph = T)

# In this case, even though we have three cases in which the slope
# is zero, Newton's method narrows in on the local minima.

# However, if we take the same function and change the starting point
# Newton's method narrows in on another value.
newton(f, -1, graph = T)

# Here it narrows in on the local maxima of the function.

# Consider another interval.
newton(f, 1, graph = T)

# Here it narrows in on the global maxima of the function.

# The main point of this display is to be thoughtful of the
# starting point you define as Newton's method will obtain
# different results.

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
