
%% Name of file and alias to distinguish from other files
\name{Newton2}
\alias{newton2}

%% Title is given Newton's method
\title{Newton's Method (Multivariate)}

\description{A multivariate local linearization technique. The incremental update equation is derived from the quadratic expansion of the Taylor series for the function with the gradient set equal to zero. We obtain a very similar equation as the univariate case, except everything is given as matrices.}

\usage{}

\arguments{}

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

\value{}

\references{Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.}

\author{Braden Zeman}

\note{See related optimization functions \link{Bisection.Rd}, \link{Secant.Rd}, \link{Newton.Rd}, \link{NelderMead.Rd}.}

\examples{}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
