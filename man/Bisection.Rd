
%% Name of file and alias to distinguish from other files
\name{Bisection}
\alias{bisection}

%% Title is given as the Bisection method
\title{Bisection Method}

%% Description of what the function does and the theory behind the bisection method
\description{A univariate, iterative root-finding method. We define an upper and lower domain boundary and then calculate the midpoint. We then replace the upper or lower boundary with the midpoint according to a selection criteria. The midpoint is then recalculated. This process is repeated until the root is found or a stopping condition is met.}

%% Identifies the function and parameters needed
\usage{bisection(f, a, b, n=100, sigma = 0.0001, graph = FALSE)}

%% Description of arguments
\arguments{
  \item{f}{Function given as an expression.}
  \item{a}{Lower domain boundary of the region.}
  \item{b}{Upper domain boundary of the region.}
  \item{n}{Number of iterations to compute. Default is 100.}
  \item{sigma}{Decimal place accuracy - stopping condition. Default is 0.0001.}
  \item{graph}{If TRUE, two graphs will be returned. Otherwise, not used.}
}

\details{The selection criteria at each iterative step:

\eqn{[a_{t+1},b_{t+1}]=[a_t,x^{(t)}]}, if \eqn{g'(a_t)g'(x^{(t)}) \leq 0}

\eqn{[a_{t+1},b_{t+1}]=[x^{(t)},b_t]}, if \eqn{g'(a_t)g'(x^{(t)})>0}
}

\value{If graph = FALSE, a list of values will be returned. In that list will be the Optima x-value, Optima y-value, the number of iterations needed to reach stopping condition (n), and the decimal level of accuracy (sigma).

If graph = TRUE, that same list of values will be returned with two graph outputs. The first graph shows to process of bisection across the iterations. The upper bound, midpoint, and lower bound are graphed at each index. In bisection, at each iterative step, the upper or lower bound is replaced by the midpoint. This graph shows this process. The second graph simply plots the function and highlights the optima point.}

\references{[1] Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.

[2] "Virtual Library of Simulation Experiments:" Optimization Test Functions and Datasets, https://www.sfu.ca/~ssurjano/optimization.html}

\author{Braden Zeman}

\note{See related optimization functions \link{Secant.Rd}, \link{Newton.Rd}, \link{Newton2.Rd}.}

\examples{
# The following examples will show how to run and utilize each of the
# arguments in the Bisection function.

# First we must define a function of interest as an expression.
f = expression(log(x)/(1+x))

# We are required to input a function, a lower boundary, and an
# upper boundary. The bisection method will then search for an optima
# within the upper and lower bounds.
bisection(f, 2, 5)

# If finding an optima is computationally intensive, we can specify
# the number of iterations we wish to compute. The default is 100,
# but that number may not be needed if stopping conditions are met.
bisection(f, 2, 5, n = 9)

# We can also specify the decimal accuracy we wish to achieve when
# finding an optima. The sigma value is directly tied to the stopping
# condition. Only the necessary number of iterations will be conducted
# until the sigma value is maintained across an iteration. Thus, the
# sigma input is prioritized over the iteration number input. The
# default sigma is four decimal places.
bisection(f, 2, 5, sigma = 0.00001)

# Note: for the faster-converging optimization methods, the true sigma
# value may be smaller than the returned sigma since the method
# gains accuracy very quickly across small iterations. Thus, treat
# sigma as a measure of "at least" this good.

# Lastly, we can chose to output a graph of the bisection method.
# Two graphs will be returned. The first will show the process of
# redefining the upper and lower bounds across iterations. The
# second will show the function, itself, with the maxima highlighted.
bisection(f, 2, 5, graph = T)

# In the following section, an example will be given to outline what
# an "optima" actual is. It is occurs when the slope or derivative of
# a function is zero. This can occur as a maxima, minima, or other.
# Consider the following:
f = expression((x-2)^3+2)
bisection(f, 1, 3, graph = T)

# As you can see, the optima point is niether the maximum or minimum
# value of the interval. It is instead where the slope is zero.
# Keep this in mind as you optimize functions and input intervals.

# Consider another example of the importance of defining intervals:
f = expression(-x^4+x^3+x^2+3)
bisection(f, -1, 1.7, graph = T)

# In this case, even though we have three cases in which the slope
# is zero, the bisection method narrows in on the global maximum.

# However, if we take the same function and shrink the interval
# the bisection method narrows in on another value.
bisection(f, -1, 0, graph = T)

# Here it narrows in on the local maxima of the entire function.

# Consider another interval.
bisection(f, -0.3, 0.4, graph = T)

# Here it narrows in on the local minima of the entire function.

# Consider another interval.
bisection(f, 0.5, 1.7, graph = T)

# And with this interval, we focus back in on the global maxima.
# The main point of this display is to be thoughtful of the
# interval you define as the bisection method will obtain
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
