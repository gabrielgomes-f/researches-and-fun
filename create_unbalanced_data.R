# Create a simulated dataset for binary regression
# Only working with 5 independent variables

normalize.data <- function(x){
  if (length(x) > 1){
    x.mean = mean(x)
    x.var = var(x)
    x.sd = sqrt(x.var)
    x.normalized = (x - x.mean)/x.sd
    return(x.normalized)
  }
  if (length(x) <= 1){
    stop("Error. The object must have length greater than 1")
  }
}


create.unbalanced.data <- function( sample.size = 1000, beta.0 = 0.5, betas = c(1, 1.5, -1, -1.5, 0.5), lambda = 1, treshold = 'random'){
  X1 <- normalize.data(runif(sample.size, -4, 4))
  X2 <- normalize.data(runif(sample.size, -3, 3))
  X3 <- normalize.data(runif(sample.size, -2, 2))
  X4 <- normalize.data(runif(sample.size, 0, 2))
  X5 <- normalize.data(rnorm(sample.size, 0, 1))
  simulated.data <- data.frame(x1, x2, x3, x4, x5)
  
  simulated.probability <- plogis( beta.0 + as.matrix(simulated.data) %*% betas) ** lambda
  
  if (treshold == 'random'){
    U <- matrix(runif(sample.size), sample.size, 1)
    Y <- as.factor( ifelse( U < simulated.probability, 1, 0))
  } else if (is.numeric(treshold) == FALSE || treshold < 0 || treshold > 1 ){
    stop("Insert a valid treshold value. It must be numeric and between 0 and 1.")
  } else {
    Y <- as.factor( ifelse (treshold < simulated.probability, 1,0))
  }
  
  simulated.data <- data.frame(simulated.data, Y, simulated.probability)
  names(simulated.data)[7] <- "p"
  
  return(simulated.data)
}
