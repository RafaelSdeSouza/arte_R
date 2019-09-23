# Collatz conjecture 

is.even <- function(x) {
  if (x %% 2 == 0) TRUE
  else FALSE
}

collatz <- function(n) {
  if (is.even(n)) n/2
  else 3 * n + 1
}

n_total <- function(n){
out <- n  
while (n != 1) {
  n <- collatz(n)
  out <- c(out, n)
}
return(out)  
}

n_total(14)
plot()


CollatzGraphing <- function(x){
  
  # loading igraph library
  library(igraph)
  
  Numbers <- 1:x
  List <- sapply(Numbers,n_total)
  
  # sorting list by length and removing empty lists
  x <- List[order(sapply(List,length),decreasing=T)]
  x <- x[sapply(x,length) >= 2]
  
  # transforming the vectors into graph format
  x <- lapply(x, rep, each = 2)
  x <- lapply(x, function(y){y[-c(1,length(y))]})
  x <- lapply(x, function(y){as.character(y)})
  
  # transforming the prepared vectors into graph objects
  x <- lapply(x, graph, directed=TRUE)
  
  # creating a union of all graph objects
  CompleteGraph <- do.call(union, x)
  
  # plotting graph
  Plot <- plot(CompleteGraph,
               vertex.size = 6,
               edge.arrow.size=0.1,
               vertex.label.cex = 0.5,
               vertex.color = "gray")
  
  # returning graph object
  return(CompleteGraph)
  
}

Graph1 <- CollatzGraphing(25)