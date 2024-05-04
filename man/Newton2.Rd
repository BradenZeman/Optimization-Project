
%% Name of file and alias to distinguish from other files
\name{Newton2}
\alias{newton2}

%% Title is given Newton's method
\title{Newton's Method (Multivariate)}

\description{A multivariate local linearization technique. The incremental update equation is derived from the quadratic expansion of the Taylor series for the function with the gradient set equal to zero. We obtain a very similar equation as the univariate case, except everything is given as matrices.}

\usage{newton2(f, a, b, n = 100, sigma = 0.0001, graph = FALSE)}

\arguments{
  \item{f}{Function given as an expression.}
  \item{a}{Initial starting x value.}
  \item{b}{Initial starting y value.}
  \item{n}{Number of iterations to compute. Default is 100.}
  \item{sigma}{Decimal place accuracy - stopping condition. Default is 0.0001.}
  \item{graph}{If TRUE, two graphs will be returned. Otherwise, not used.}
}

\details{The selection criteria at each iterative step is given by:

\eqn{\bf{x}^{(t+1)}=\bf{x}^{(t)}-\bf{g}''(\bf{x}^{(t)})^{-1}\bf{g}'(\bf{x}^{(t)})}


This updating equation was derived by first approximating the maxima critical value using the quadratic Taylor series expansion:

\eqn{\bf g(x^*)=g(x^{(t)})+(x^*-x^{(t)})^Tg'(x^{(t)})+\frac{1}{2}(x^*-x^{(t)})^Tg''(x^{(t)})(x^*-x^{(t)})}

Next we "maximize" this quadratic Taylor series function by taking the derivative of both sides with respect to \eqn{\bf x^*} and setting the gradient equal to zero.

\eqn{\bf \frac{d}{dx^*}g(x^*)=\frac{d}{dx^*}(g(x^{(t)})+(x^*-x^{(t)})^Tg'(x^{(t)})+\frac{1}{2}(x^*-x^{(t)})^Tg''(x^{(t)})(x^*-x^{(t)}))}

\eqn{\bf g'(x^*)=g'(x^{(t)})+g''(x^{(t)})(x^*-x^{(t)})=0}

Now we simply do some rearranging.

\eqn{\bf g'(x^{(t)})=-g''(x^{(t)})(x^*-x^{(t)})}

\eqn{\bf -g''(x^{(t)})^{-1}g'(x^{(t)})=(x^*-x^{(t)})}

\eqn{\bf x^*=x^{(t)}-g''(x^{(t)})^{-1}g'(x^{(t)})}

Lastly, we rename \eqn{\bf x^*} to \eqn{\bf x^{(t+1)}}. Now we have the updating equation.

}

\value{If graph = FALSE, a list of values will be returned. In that list will be the Maximum x-value, Maximum y-value, Maximum z-value, the number of iterations needed to reach stopping condition (n), and the decimal level of accuracy (sigma).

If graph = TRUE, that same list of values will be returned with two graphs. The first graph plots a 3D surface image with the optima point highlighted. The second graph plots the contour lines with the optima hihglighted.}

\references{[1] Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.

[2] "Virtual Library of Simulation Experiments:" Optimization Test Functions and Datasets, \href{https://www.sfu.ca/~ssurjano/optimization.html}{https://www.sfu.ca/~ssurjano/optimization.html}}

\author{Braden Zeman}

\note{See related optimization functions \link{bisection}, \link{secant}, \link{newton}.}

\examples{
# The following examples will show how to run and utilize each of the
# arguments in the Newton function.

# First we must define a function of interest as an expression.
f = expression(sqrt(4-x^2-y^2))

# At a minimum, we must input a starting x and y value in order to
# start the iterative process. Make sure its within a domain and
# range which can be evaluated.
newton2(f, 0.5, 1)

# We can also specify the number of iterations if necessary.
# Newton's method seems to converge very quickly so changing
# this value to anything greater than 100 is very unlikely.
newton2(f, 0.5, 1, n = 2)

# We can also specify the decimal accuracy we wish to achieve
# when finding the optima. Since Newton's method converges very
# quickly, we often do not need to change this value. The decimal
# accuracy stopping condition supercedes the iteration number and
# will stop when the sigma value is obtain.
newton2(f, 0.5, 1, sigma = 0.1)

# Lastly, we can chose to output two graphs. The first graph
# will be a 3D surface plot. The second graph will be a contour
# plot. In both the optima will be highlighted.
newton2(f, 0.5, 1, graph = T)

# One of the last things of importance is to discuss the
# sensitivity of Newton's method. When searching over
# 3D space, it may find local optima instead of global optima
# based on the defined starting values. For instance, consider:
f = expression(-(1+cos(12*sqrt(x^2+y^2)))/(0.5*(x^2+y^2)+2))
newton2(f, 1, 1, graph = T)

# Due to the oscillating nature of the function, there are
# a plethora of local minima and maxima that Newton's method
# will try to search for. Ultimately, it found a local minima.

# If we change the starting point, we can obtain a new optima.
newton2(f, 0.5, 0.5, graph = T)

# This time it is a local maxima for the function.

# Let's try one more time.
newton2(f, 0.05, 0.05, graph = T)

# Now we've actually identified the global minima. The main
# point of this demonstration was to showcase that initial
# starting points matter greatly. Newton's method searches for
# optima whether local or global nearby the starting point.

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
