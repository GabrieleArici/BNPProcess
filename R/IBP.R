# Indian Buffet Process

if (!require("methods"))
  require("methods", repos="http://cran.us.r-project.org")

if (!require("ggplot2"))
  require("ggplot2", repos="http://cran.us.r-project.org")

if (!require("reshape2"))
  require("reshape2", repos="http://cran.us.r-project.org")

if (!require("R.oo"))
  require("R.oo", repos="http://cran.us.r-project.org")

library(methods)
library(ggplot2)
library(reshape2)
library(R.oo)

# Check object validity
check_IBP <- function(object)
{
  errors <- character()

  length_lambda <- length(object@lambda)
  value_lambda <- object@lambda
  if (length_lambda > 1)
  {
    msg <- paste("lambda is length ", length_lambda, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if (value_lambda <= 0)
  {
    msg <- paste("Invalid lambda value", value_lambda, " provided, must be greater than 0", sep = "")
    errors <- c(errors, msg)
  }

  length_n <- length(object@n)
  value_n <- object@n
  if (length_n > 1)
  {
    msg <- paste("n is length ", length_n, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if (value_n <= 1 || value_n%%1 != 0)
  {
    msg <- paste("Invalid n value", value_n, " provided, must be an integer greater than 1", sep = "")
    errors <- c(errors, msg)
  }

  length_theta <- length(object@theta)
  value_theta <- object@theta
  if (length_theta > 1)
  {
    msg <- paste("theta is length ", length_theta, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if (value_theta < 1)
  {
    msg <- paste("Invalid theta value", value_theta, " provided, must be equal to or greater than 1", sep = "")
    errors <- c(errors, msg)
  }

  length_alpha <- length(object@alpha)
  value_alpha <- object@alpha
  if (length_theta > 1)
  {
    msg <- paste("alpha is length ", length_theta, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if (value_alpha < 0 || value_alpha > 1)
  {
    msg <- paste("Invalid alpha value", value_theta, " provided, must be between 0 and 1", sep = "")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

setClass("IndianBuffet",
         representation(lambda = "numeric",
                        n = "numeric",
                        theta = "numeric",
                        alpha = "numeric",
                        expected = "numeric",
                        features = "list"),
         prototype = list(lambda= double(1),
                          n = double(1),
                          theta = double(1),
                          alpha = double(1),
                          expected = double(1),
                          features = list()),
         validity = check_IBP,
         sealed=FALSE
)

#getSlots("IndianBuffet")

IBP <- function(lambda, n, theta = 1, alpha = 0)
{

  IB_obj <- new("IndianBuffet", lambda=lambda, n=n, theta=theta, alpha=alpha)

  pre_x1 <- 0
  while(pre_x1==0)
    pre_x1 <- rpois(n = 1, lambda = lambda)

  output <- list()
  output[[1]] <- runif(pre_x1, 0, 1)
  exp <- 0

  for(i in 2:n)
  {
    exp <- exp + 1/i
    seen <- numeric(length(output[[i-1]]))
    copy <- numeric(length(output[[i-1]]))

    for(j in 1:(i-1))
    {
      dt <- as.numeric(ceiling(output[[j]]))
      seen[1:length(dt)] <- seen[1:length(dt)] + dt
    }

    if(theta == 1 && alpha == 0)
    {
      copy[((runif(length(seen),0,1)) < seen/(i))==TRUE] <- 1
      output[[i]] <- c(copy,runif(rpois(n = 1, lambda = lambda/i), 0, 1))

    }
    else
    {
      copy[((runif(length(seen),0,1)) < ((seen-alpha)/(theta + i -1)))==TRUE] <- 1
      Lprime <- lambda * (gamma(theta+1) * gamma(theta + alpha - 1 + i))/
        (gamma(theta + i)*gamma(theta + alpha))
      output[[i]] <- c(copy,runif(rpois(n = 1, lambda = Lprime), 0, 1))
    }
  }

  IB_obj@expected <- exp*lambda
  IB_obj@features <- output
  return (IB_obj)
}

#Indian <- IBP(lambda = 5, n = 50, theta = 1, alpha = 0)
#Indian

setMethod("plot", signature(x="IndianBuffet", y="missing"), function(x, y, ...)
{
  tmp <- matrix(0,x@n,length(x@features[[x@n]]))
  for(i in 1:x@n)
    tmp[i,1:length(x@features[[i]])] <- x@features[[i]]
  tmp <- ceiling(tmp)

  P1 <- ggplot(melt(tmp), aes(Var2, Var1, fill=factor(value))) +
    xlab("Features") + ylab("Individuals") +
    geom_raster() +
    ggtitle("Features development") +
    scale_fill_manual(name="Legend", values=c("darkblue","lightblue"), labels=c("0 - Not selected","1 - Selected")) +
    scale_y_reverse()
  return (P1)
})
#plot(Indian)

setGeneric("findCustomer", function(object, id)
{
  standardGeneric("findCustomer")
})
setMethod("findCustomer", signature(object="IndianBuffet"), function(object, id)
{
  return (object@features[[id]])
})
#table_id <- findCustomer(Indian, id = 6)
#table_id

setGeneric("differentDishes", function(object)
{
  standardGeneric("differentDishes")
})
setMethod("differentDishes", signature(object="IndianBuffet"), function(object)
{
  tmp <- as.data.frame(matrix(0, nrow = object@n, ncol = 2))
  tmp[,1] <- seq(1, object@n, 1)
  for(i in 1:object@n)
    tmp[i,2] <- sum(object@features[[i]]!=0)
  names(tmp) <- c("Individuals","Features")
  return(tmp)
})
#differentDishes(Indian)

setGeneric("tableS4", function(object, ...)
{
  standardGeneric("tableS4")
})
setMethod("tableS4", "IndianBuffet", function(object, ...)
{
  tmp <- differentDishes(object)
  return (table(tmp[,2]))
})
setMethodS3("table", class="IndianBuffet", function(object, ...) tableS4(object, ...))
#table(Indian)

setGeneric("progression", function(object)
{
  standardGeneric("progression")
})
setMethod("progression", signature(object="IndianBuffet"), function(object)
{
  tmp <- differentDishes(object)

  P1 <- ggplot(tmp, aes(Individuals,Features)) +
    geom_line(aes(colour = Features)) +
    scale_colour_gradient(low="red") +
    xlab("Individuals") +
    ylab("Number of features") +
    ggtitle("Features per Individual")
  return (P1)
})
#progression(Indian)

setMethod("summary",  signature(object="IndianBuffet"), function(object, ...)
{
  dt <- data.frame(strings = c("Mass parameter ",
                               "Number of customers ",
                               "Concentration parameter ",
                               "Discount parameter ",
                               "Observed features ",
                               "Expected features "
  ),
  sep = c(":",":",":",":",":",":"),
  numbers = c(object@lambda,
              object@n,
              object@theta,
              object@alpha,
              length(object@features[[object@n]]),
              round(object@expected,3)), row.names = NULL)
  names(dt) <- c(" ", " ", " ")

  dt <- format(dt, justify = "left")
  return(dt)
})
#summary(Indian)
