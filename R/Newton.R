

# This function calculates optima using Newton's method
# The inputs include:
# (f = the function of interest given as an expression)
# (a = starting point), (n = number of iterations)
# (sigma = number of decimal places of accuracy)
# (graph = whether or not you want graph output included)

newton = function(f, a, n=100, sigma = 0.0001, graph = FALSE) {

  # This code evaluates the function at a specified x value
  g = function(x){eval(f)}

  # Error message if function is not an expression
  if (is.expression(f)) {
  } else {
    stop("f is not an expression.")
  }

  # Error message if x input is not a number
  if (is.double(a)) {
  } else {
    stop("a (x input) is not a number.")
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

  # Error message if x input is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(a)))) {
    stop("a (x input) is outside the domain of the function")
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
  dx = D(f, "x")

  # This code calculates the second derivative of the function
  dx2 = D(dx, "x")

  # This code evaluates the derivative at a specified x value
  evaldx = function(x){eval(dx)}

  # This code evaluates the second derivative at a specified x value
  evaldx2 = function(x){eval(dx2)}

  # We define needed variables and vectors to be updated throughout process
  # xValues, yValues are needed for graphing
  # index keeps track of the number of iterations
  # b is just an initial value of little importance to start the stopping condition
  index = 0
  xValues = c()
  yValues = c()
  b = a + 1

  # Loop that updates using Newton's method (n times)
  for (x in 1:n) {

    # Keeps track of all of the points in Newton approximation as we iterate
    xValues = append(xValues, a)
    yValues = append(yValues, g(a))

    # This is the stopping condition that will break away from the loop
    # if a certain decimal accuracy is achieved
    if (abs(a-b)>sigma) {

      # Updates the index to keep track of number of iterations
      index = index + 1

      # The following code is the Newton updating equation
      xNew = a - (evaldx(a)/evaldx2(a))

      b = a
      a = xNew

    } else {
      break
    }
  }


  # This creates the domain of the plot
  if (a>b) {
    xdomain <- seq(b-1, a+1, 0.001)
    ydomain = c()
  } else {
    xdomain <- seq(a-1, b+1, 0.001)
    ydomain = c()
  }

  # Calculates the maximum y value
  Yvalue = g(xNew)

  # Creates a dataframe of the relevant Newton points at each step
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


