
library(plotly)

# This function calculates optima in 3D space using Newton's method
# The inputs include:
# (f = the function of interest given as an expression)
# (a = starting x value), (b = starting y value), (n = number of iterations)
# (sigma = number of decimal places of accuracy)
# (graph = whether or not you want graph output included)

newton2 = function(f, a, b, n = 100, sigma = 0.0001, graph = FALSE) {

  # This function evaluates the function at the specified x and y value
  g = function(x, y){eval(f)}

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

  # Error message if y input is not a number
  if (is.double(b)) {
  } else {
    stop("b (y input) is not a number.")
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

  # Error message if x or y input is outside a reasonable domain of evaluation
  if (suppressWarnings(is.na(g(a, b)))) {
    stop("a (x input) and/or b (y input) is outside the domain and/or range of function")
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

  # This code calculates the firstpartial derivative with respect to x
  dx = D(f, "x")

  # This code calculates the first partial derivative with respect to y
  dy = D(f, "y")

  # This code calculates the second partial derivative with respect to x
  dx2 = D(dx, "x")

  # This code calculates the second partial derivative with respect to y
  dy2 = D(dy, "y")

  # This code calculates the second partial derivative first with respect
  # to x, then with respect to y
  dxdy = D(dx, "y")

  # This code calculates the second partial derivative first with respect
  # to y, then with respect to x
  dydx = D(dy, "x")

  # This function evaluates the first partial derivative with respect to x
  evaldx = function(x, y){eval(dx)}

  # This function evaluates the first partial derivative with respect to y
  evaldy = function(x, y){eval(dy)}

  # This function evaluates the second partial derivative with respect to x
  evaldx2 = function(x, y){eval(dx2)}

  # This function evaluates the second partial derivative with respect to y
  evaldy2 = function(x, y){eval(dy2)}

  # This function evaluates the second partial derivative first with respect
  # to x, then with respect to y
  evaldxdy = function(x, y){eval(dxdy)}

  # This function evaluates the second partial derivative first with respect
  # to y, then with respect to x
  evaldydx = function(x, y){eval(dydx)}

  # Takes the x and y inputs and puts them into a matrix
  xOld = matrix(c(a, b), nrow = 2)

  # Initializes a comparison matrix for error calculation
  xPrior = matrix(c(0, 0), nrow = 2)

  # Creates index for iteration count
  index = 0

  # Initializes lists to track individual points as we iterate
  xValues = c()
  yValues = c()
  zValues = c()

  # Loop that updates using Newton's method (n times)
  for (x in 1:n) {

    # Keeps track of individual points as we iterate
    xValues = append(xValues, xOld[1])
    yValues = append(yValues, xOld[2])
    zValues = append(zValues, g(xOld[1], xOld[2]))

    # This is the stopping condition that will break away from the loop
    if (sqrt((xOld[1]-xPrior[1])^2 + (xOld[2]-xPrior[2])^2) > sigma) {

      # Updates the index to keep track of number of iterations
      index = index + 1

      # Calculates the elements and forms the Hessian matrix
      dX2 = evaldx2(xOld[1], xOld[2])
      dXdY = evaldxdy(xOld[1], xOld[2])
      dYdX = evaldydx(xOld[1], xOld[2])
      dY2 = evaldx2(xOld[1], xOld[2])
      H = matrix(c(dX2, dYdX, dXdY, dY2), nrow = 2)

      # Calculates the elements and forms a matrix of the first partial derivatives
      dX = evaldx(xOld[1], xOld[2])
      dY = evaldy(xOld[1], xOld[2])
      P = matrix(c(dX, dY), nrow = 2)

      # The following code is the Newton updating equation
      xNew = xOld - solve(H)%*%P

      # Updates for next iteration
      xPrior = matrix(c(xOld[1], xOld[2]), nrow = 2)
      xOld = matrix(c(xNew[1], xNew[2]), nrow = 2)

    } else {
      break
    }
  }

  # Calculates the maximum z value
  z = g(xNew[1], xNew[2])

  # Creates a dataframe of the relevant output
  graphing = data.frame(xValues, yValues, zValues)
  results = data.frame(Optima_X = xNew[1], Optima_Y = xNew[2], Optima_Z = z,
                       Iterations = index, Sigma = sigma)

  # If the user specified TRUE, then two graphs will be plotted
  if (isTRUE(graph)) {

    # This creates the domain of the plot
    xdomain = seq(xNew[1]-1, xNew[1]+1, 0.01)
    ydomain = seq(xNew[2]-1, xNew[2]+1, 0.01)
    zdomain = outer(xdomain, ydomain, g)

    # Graphs a 3D surface plot with the max point
    print(plot_ly(x = xdomain, y = ydomain, z = zdomain, type = "surface")
          %>% add_trace(x = xNew[1], y = xNew[2], z = z, type = "scatter3d", mode = "markers"))

    # Graphs a contour plot with the max point
    suppressWarnings(print(plot_ly(x = xdomain, y = ydomain, z = zdomain, type = "contour")
                           %>% add_trace(x = xNew[1], y = xNew[2], type = "scatter", mode = "markers")))

    # Returns the results and graphing data
    return(list(results, graphing))

  } else {

    # If graphing was not specified, just results are returned
    return(results)
  }

}
