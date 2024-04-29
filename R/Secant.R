

# This function calculates optima using the secant method
# The inputs include:
# (f = the function of interest given as an expression)
# (a = first point), (b = second point), (n = number of iterations)
# (sigma = number of decimal places of accuracy)
# (graph = whether or not you want graph output included)

secant = function(f, a, b, n=100, sigma = 0.0001, graph = FALSE) {

  # This code evaluates the function at a specified x value
  g = function(x){eval(f)}

  # Error message if function is not an expression
  if (is.expression(f)) {
  } else {
    stop("f is not an expression.")
  }

  # Error message if first point is not a number
  if (is.double(a)) {
  } else {
    stop("a (first point) is not a number.")
  }

  # Error message if second point is not a number
  if (is.double(b)) {
  } else {
    stop("b (second point) is not a number.")
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

  # Error message if first point is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(a)))) {
    stop("a (first point) is outside the domain of the function")
  } else {
  }

  # Error message if second point is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(b)))) {
    stop("b (second point) is outside the domain of the function")
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
  # xValues, yValues, xdomain, ydomain are needed for graphing
  # index keeps track of the number of iterations
  index = 0
  xValues = c()
  yValues = c()
  xdomain <- seq(a, b, 0.001)
  ydomain = c()

  # Initializes the original point
  xValues = append(xValues, a)
  yValues = append(yValues, g(a))

  # Loop that updates using the secant method (n times)
  for (x in 1:n) {

    # Keeps track of all of the secant points as we iterate
    xValues = append(xValues, b)
    yValues = append(yValues, g(b))

    # This is the stopping condition that will break away from the loop
    # if a certain decimal accuracy is achieved
    if (abs(a-b)>sigma) {

      # Updates the index to keep track of number of iterations
      index = index + 1

      # Calculates the derivative at the first point and
      # the derivative at the second point
      dxBefore = evalDerivative(a)
      dxNow = evalDerivative(b)

      # The following code is the secant updating equation
      xNew = b - dxNow*((b-a)/(dxNow-dxBefore))

      a = b
      b = xNew

    } else {
      break
    }
  }

  # Calculates the maximum y value
  Yvalue = g(xNew)

  # Creates a dataframe of the relevant secant points at each step
  # as well as the important optima information
  graphing = data.frame(xValues, yValues)
  results = data.frame(Maximum_X = xNew, Maximum_Y = Yvalue, Iterations = index, Sigma = sigma)

  # If the user specified TRUE, then a graph will be plotted
  if (isTRUE(graph)) {

    # Establishes the range of the plot
    for (x in 1:length(xdomain)) {
      ydomain = append(ydomain, g(xdomain[x]))
    }

    # Creates a graph of the function itself and the identified maximum
    plot(ydomain~xdomain, type="l", xlab = "x", ylab = "y", main = "Plot of Function")
    points(xNew, Yvalue, col = "red", pch = 16)

    # Returns the results and graphing data
    return(list(results, graphing))

  } else {

    # If graphing was not specified, just results are returned
    return(results)
  }
}

