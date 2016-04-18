# Chinese Restaurant Process

# Library to be needed
library(methods)
library(ggplot2)
library(R.oo)

# Checkin function
check_CRP <- function(object)
{
  errors <- character()

  #Check concentration parameter length and value
  value_theta <- object@theta
  length_theta <- length(object@theta)
  if(length_theta > 1)
  {
    msg <- paste("Theta is length ", length_theta, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if(value_theta < 0)
  {
    msg <- paste("Invalid theta value. ", value_theta, " provided, should be greater than 0", sep = "")
    errors <- c(errors, msg)
  }

  #Check number of customers length and value
  value_n <- object@n
  length_n <- length(object@n)
  if(length_n > 1)
  {
    msg <- paste("Theta is length ", length_n, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  else if(value_n <= 1)
  {
    msg <- paste("Invalid n value. ", value_n, " provided, should be greater than 1", sep = "")
    errors <- c(errors, msg)
  }

  if(length(errors) == 0) TRUE else errors
}

# Setting the class
setClass("ChineseRestaurant", representation(theta="numeric",
                                             n="numeric",
                                             ntables= "numeric",
                                             tables="list",
                                             expected="numeric"),
         prototype(theta=double(1),
                   n=double(1),
                   ntables=double(1),
                   tables=list(),
                   expected=double(1)),
         validity=check_CRP,
         sealed=FALSE)

#getSlots("ChineseRestaurant")

# Definition of the process
CRP <- function(theta, n)
{

  # Invoke the creation of a new object of class ChineseRestaurant..
  CR_obj <- new("ChineseRestaurant", theta=theta, n=n)

  tables <- list()
  tables[[1]] <- 1
  Exp <- 1

  # ..then fill the tables
  for(i in 2:n)
  {
    probs <- numeric(length(tables) + 1)
    probs[1:length(probs)-1] <- as.numeric(sapply(tables, length))/(theta + i - 1)
    probs[length(probs)] <- theta/(theta + i - 1)
    new <- sample.int(length(tables)+1, size = 1, prob = probs)
    if(new == length(tables)+1)
      tables[[new]] <- i
    else
      tables[[new]] <- c(tables[[new]], i)

    Exp <- Exp + theta/(theta + i - 1)
  }

  # Fill object attributes
  CR_obj@ntables <- length(tables)
  CR_obj@tables <- tables
  CR_obj@expected <- Exp

  return (CR_obj)
}

#Chinese <- CRP(theta = 5, n = 100)
#Chinese@theta
#Chinese@n
#Chinese@ntables
#Chinese@expected

# Define the "table" method
# Define it as a S4 method, then convert it to S3
setGeneric("tableS4", function(object, ...)
{
  standardGeneric("tableS4")
})
setMethod("tableS4", "ChineseRestaurant", function(object, ...)
{
  x <- object@tables
  y <- object@ntables
  result <- as.data.frame(lapply(x,length))
  colnames(result) <- seq(1,y,1)
  rownames(result) <- "Frequency"
  return(result)
  return (table(tmp[,2]))
})
setMethodS3("table", class="ChineseRestaurant", function(object, ...) tableS4(object, ...))
#table(Chinese)

# Define the "plot" method
# Return an histogram with number of customers per table
setMethod("plot", signature(x="ChineseRestaurant", y="missing"), function(x, y, ...)
{

  Part <- numeric(length(x@tables))
  for(i in 1:length(Part))
    Part[i] <- length(x@tables[[i]])

  Data <- data.frame(ID=seq(1,length(Part),1), Customers=Part)

  plot <- qplot(x=ID, y=Customers, data=Data, stat="identity", geom="bar",
                main="Customers per Table", ylab="Number of customers",
                xlab="Tables", fill=Customers)
  return (plot)
})
#plot(Chinese)

# Define the "summary" method
setMethod("summary",  signature(object="ChineseRestaurant"), function(object, ...)
{
  dt <- data.frame(strings = c("Concentration parameter ",
                               "Number of customers ",
                               "Number of generated tables ",
                               "Number of expected tables "
  ),
  sep = c(":",":",":",":"),
  numbers = c(object@theta,
              object@n,
              object@ntables,
              round(object@expected,3)), row.names = NULL)
  names(dt) <- c(" ", " ", " ")

  dt <- format(dt, justify = "left")
  return(dt)
  #print(dt, row.names=FALSE)
  #occupance(object)
})
#tb <- summary(Chinese)

# Define di "findCustomer" method
# Given a customer id, return the table
setGeneric("findCustomer", function(object, id)
{
  standardGeneric("findCustomer")
})
setMethod("findCustomer", signature(object="ChineseRestaurant"), function(object, id)
{
  x <- object@tables
  for(i in 1:min(id, object@ntables))
    if(id %in% x[[i]])
      return (i)
})
#table_id <- findCustomer(Chinese, 90)
#table_id
