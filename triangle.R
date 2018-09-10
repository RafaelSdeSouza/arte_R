#R Script
IterateTriangle <- function(A){
  B <- cbind(A,A,A);
  C <- cbind(0*A,A,0*A);
  D <- rbind(C,C,B);
  return(D);
}




for (i in 1:5){
  T <- matrix(1,1,1)
  for (i in 1:i) T <- IterateTriangle(T);
  }
image(T,col=c("black","white"),axes=FALSE);
legend("topleft", legend = "© Rafael S. de Souza", bty = "n", text.col = "white",cex=1)



T <- matrix(1,1,1)
for (i in 1:i) 
T <- IterateTriangle(T);
image(T,col=c("red2","cyan3"),axes=FALSE);




SierpinskiCarpet <- function(k){
  Iterate <- function(M){
    A <- cbind(M,M,M);
    B <- cbind(M,0*M,M);
    return(rbind(A,B,A))
  }
  
IterateSquare <- function(A){
  B <- cbind(A,A,A);
  C <- cbind(A,0*A,A);
  D <- rbind(B,C,B);
  return(D);
}

#N <-c("R","A","F","A","E","L")
#par(mfrow=c(3,2))
for (i in 1:5){
  T <- as.matrix(1)
  for (i in 1:i) T <- IterateSquare(T);
  #  text(0,1,N[i],col="red4") 
}
image(T,col=c("white","blue"),axes=FALSE,lwd=20);

sierpinski5 <- function(x) {
  m <- matrix(c(0.382,0, 0,0.382), nrow=2)
  o <- matrix(c(0,0, 0.618,0, 0.809,0.588, 0.309,0.951, -0.191,0.588), nrow=2)
  fold(o, function(a,b) cbind(m %*% x + a, b))
}