
%% Name of file and alias to distinguish from other files
\name{NelderMead}
\alias{neldermead}

%% Title is given Newton's method
\title{Nelder-Mead Algorithm}

\description{A multivariate local linearization technique that does not involve any derivatives. We start by computing a triangular boundary that lays overtop the contours of the funciton. We then rank the three vertices as Best, Bad, Worst (i.e. largest to smallest function value). Along the "best" face of the triangle (i.e. the side that connects the Best and Bad vertices), we calculate the centroid. From there we define an algorithm that determines whether or not the triangle undergoes reflection, expansion, contraction, or shrinkage. This repeats until a stopping condition is met.}

\usage{}

\arguments{}

\details{}

\value{}

\references{Givens, Geof H. Computational Statistics. 2nd ed., John Wiley & Sons Inc., 2013.}

\author{Braden Zeman}

\note{See related optimization functions \link{Bisection.Rd}, \link{Secant.Rd}, \link{Newton.Rd}, \link{Newton2.Rd}.}

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
