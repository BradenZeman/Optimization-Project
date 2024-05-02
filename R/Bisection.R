
# This function calculates optima using the bisection method
# The inputs include:
# (f = the function of interest given as an expression)
# (a = lower bound), (b = upper bound), (n = number of iterations)
# (sigma = number of decimal places of accuracy)
# (graph = whether or not you want graph output included)

bisection = function(f, a, b, n=100, sigma = 0.0001, graph = FALSE) {

  # This code evaluates the function at a specified x value
  g = function(x){eval(f)}

  # Error message if function is not an expression
  if (is.expression(f)) {
  } else {
    stop("f is not an expression.")
  }

  # Error message if lower bound is not a number
  if (is.double(a)) {
  } else {
    stop("a (lower bound) is not a number.")
  }

  # Error message if upper bound is not a number
  if (is.double(b)) {
  } else {
    stop("b (upper bound) is not a number.")
  }

  # Error message if n is not an integer
  if (is.numeric(n)) {
  } else {
    stop("n (iterations) is not an integer.")
  }

  # Error message if sigma is not a number
  if (is.double(sigma)) {
  } else {
    stop("sigma (error) is not a number.")
  }

  # Error message if lower bound is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(a)))) {
    stop("a (lower bound) is outside the domain of the function")
  } else {
  }

  # Error message if upper bound is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(b)))) {
    stop("b (upper bound) is outside the domain of the function")
  } else {
  }

  # Error Message if graph is not a boolean expression
  if (isTRUE(graph)) {
  } else {
    if (isFALSE(graph)) {
    } else {
      stop("graph is not a boolean statement.")
    }
  }

  # This code calculates the first derivative of the function
  derivative = D(f, "x")

  # This code evaluates the derivative at a specified x value
  evalDerivative = function(x){eval(derivative)}

  # We define needed variables and vectors to be updated throughout process
  # graphLow, graphUp, midpoint, xdomain, ydomain are needed for graphing
  # index keeps track of the number of iterations
  index = 0
  graphLow = c()
  graphUp = c()
  midpoint = c()
  xdomain <- seq(a, b, 0.001)
  ydomain = c()

  # Loop that updates using the bisection method (n times)
  for (x in 1:n) {

    # Keeps track of all of the bounds or midpoints as we iterate
    mid = 0.5*(a+b)
    graphLow = append(graphLow, a)
    graphUp = append(graphUp, b)
    midpoint = append(midpoint, mid)

    # This is the stopping condition that will break away from the loop
    # if a certain decimal accuracy is achieved
    if (abs(a-b)>sigma) {

      # Updates the index to keep track of number of iterations
      index = index + 1

      # Calculation of derivative at (a) and (mid) positions
      mid = 0.5*(a+b)
      dA = evalDerivative(a)
      dmid = evalDerivative(mid)

      # This is the bisection selection criteria to replace lower
      # or upper boundary
      if (dA*dmid <= 0) {

        b = mid

      } else {

        a = mid

      }
    } else {
      break
    }
  }

  # Calculates the maximum x and y value
  ans = (a+b)/2
  Yvalue = g(ans)

  # Creates a dataframe of the relevant bisection points at each step
  # as well as the important optima information
  graphing = data.frame(graphLow, midpoint, graphUp)
  results = data.frame(Optima_X = ans, Optima_Y = Yvalue, Iterations = index, Sigma = sigma)

  # If the user specified TRUE, then a graph will be plotted
  if (isTRUE(graph)) {

    # This code graphs the series of upper bounds, lower bounds, and midpoints across iterations
    # so that you can visualize how the bisection method actually works
    plot(graphLow~1, ylim = c(graphLow[1], graphUp[1]), col = "green", xlab = "Iteration Number",
         ylab = "Upper, Middle, Lower Bounds", main = "Bisection Method",
         par(mar=c(7.5, 4.5, 4.5, 2.5)))
    points(graphUp, col = "red")
    points(midpoint, col = "blue")
    legend(x="bottom", xpd = TRUE, horiz = TRUE, inset = c(0, -0.35),
           legend=c("Lower Bound", "Midpoint", "Upper Bound"),
           fill = c("green", "blue", "red"))

    # Establishes a domain and range for a graph of the function
    for (x in 1:length(xdomain)) {
      ydomain = append(ydomain, g(xdomain[x]))
    }

    # Creates another graph of the function itself and the identified maximum
    plot(ydomain~xdomain, type="l", xlab = "x", ylab = "y", main = "Plot of Function")
    points(ans, Yvalue, col = "red", pch = 16)

    # Returns the results and graphing data
    return(list(results, graphing))

  } else {

    # If graphing was not specified, just results are returned
    return(results)
  }
}

