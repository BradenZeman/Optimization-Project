
%% Name of file and alias to distinguish from other files
\name{Secant}
\alias{secant}

%% Title is given as the Secant method
\title{Secant Method}

%% Description of what the function does and the theory behind the Secant method
\description{A univariate, iterative root-finding method. We define two initial starting points and then calculate the secant line between them. Afterwards, we find the x-value at which the secant line is equal to zero. We then calculate a new secant line between this new point and the previous domain point. This process is repeated until the root is found or a stopping condition is met.}

%% Identifies the function and parameters needed
\usage{secant(f, a, b, n=100, sigma = 0.0001, graph = FALSE)}

%% Description of arguments
\arguments{
  \item{f}{Function given as an expression.}
  \item{a}{First domain value required for secant line approximation.}
  \item{b}{Second domain value required for secant line approximation.}
  \item{n}{Number of iterations to compute. Default is 100.}
  \item{sigma}{Decimal place accuracy - stopping condition. Default is 0.0001.}
  \item{graph}{If TRUE, a graph will be returned. Otherwise, not used.}
}

\details{The selection criteria at each iterative step is given by:

\eqn{x^{(t+1)}=x^{(t)}-g'(x^{(t)})\frac{x^{(t)}-x^{(t-1)}}{g'(x^{(t)})-g'(x^{(t-1)})}}

This updating equation comes from Newton's method but attempts to replace the second derivative if its calculation proves too difficult. Consider Newton's method (\link{Newton.Rd}) below:

\eqn{x^{(t+1)}=x^{(t)}-\frac{g'(x^{(t)})}{g''(x^{(t)})}=x^{(t)}-g'(x^{(t)})\frac{1}{g''(x^{(t)})}}

Now consider the discrete-difference approximation of \eqn{g''(x^{(t)})} and the following algebriac manipulation:

\eqn{g''(x^{(t)})=\frac{g'(x^{(t)})-g'(x^{(t-1)})}{x^{(t)}-x^{(t-1)}}}

\eqn{\frac{1}{g''(x^{(t)})}=\frac{x^{(t)}-x^{(t-1)}}{g'(x^{(t)})-g'(x^{(t-1)})}}

And thus, we can slot this back into Newton's method and get the updating equation.

\eqn{x^{(t+1)}=x^{(t)}-g'(x^{(t)})\frac{x^{(t)}-x^{(t-1)}}{g'(x^{(t)})-g'(x^{(t-1)})}}

}

\value{If graph = FALSE, a list of values will be returned. In that list will be the Maximum x-value, Maximum y-value, the number of iterations needed to reach stopping condition (n), and the decimal level of accuracy (sigma).

If graph = TRUE, that same list of values will be returned with a graph. The graph plots the function and highlights the optima point.}

\references{Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.}

\author{Braden Zeman}

\note{See related optimization functions \link{Bisection.Rd}, \link{Newton.Rd}, \link{Newton2.Rd}, \link{NelderMead.Rd}.}

\examples{
# The following examples will show how to run and utilize each of the
# arguments in the Secant function.

# First we must define a function of interest as an expression.
f = expression(log(x)/(1+x))

# At the minimum, we must input a function and two points in order to
# calculate the secant line. Using the secant lines, we will incrementally
# approach the optima for the function.
secant(f, 2, 5)

# If finding an optima is computationally intensive, we can specify
# the number of iterations we wish to compute. The default is 100,
# but that number may not be needed if stopping conditions are met.
secant(f, 2, 5, n = 9)

# We can also specify the decimal accuracy we wish to achieve when
# finding an optima. The sigma value is directly tied to the stopping
# condition. Only the necessary number of iterations will be conducted
# until the sigma value is maintained across an iteration. Thus, the
# sigma input is prioritized over the iteration number input. The
# default sigma is four decimal places.
secant(f, 2, 5, sigma = 0.00001)

# Lastly, we can chose to output a graph of the secant method. The
# graph will plot the function and highlight the optima point.
secant(f, 2, 5, graph = T)
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
