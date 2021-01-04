require(lattice)

make.Stereo <- function(Z, Groups, Screen1=list(z = 20, x = -70, y = 3),
                        Screen2=list(z = 20, x = -70, y = 0),
                        Main="Stereo", asp="Equal",
                        Xlab="X", Ylab="Y",Zlab="Z", pch=16) {
   Z <- data.frame(Z)
   dimnames(Z) <- list(1:dim(Z)[1], c("Z1", "Z2", "Z3"))
   if (asp == "Equal") {
      X.range <- c(min(apply(Z,2,range)[1,]), max(apply(Z,2,range)[2,])) 
      Y.range <- X.range 
      Z.range <- X.range 
   } else {
      X.range <- range(Z[,1])   
      Y.range <- range(Z[,2])   
      Z.range <- range(Z[,3])   
   }
   print(cloud(Z3 ~ Z1 * Z2, data = Z, pch=pch,
            cex = .8, perspective = FALSE, 
            groups=Groups,
            subpanel = panel.superpose,
            main = Main,
            screen = Screen1,
            xlim=X.range,ylim=Y.range,zlim=Z.range,
            xlab=Xlab, ylab=Ylab, zlab=Zlab),
            split = c(1,1,2,1), more = TRUE)
   print(cloud(Z3 ~ Z1 * Z2, data = Z, pch=pch,
            cex = .8, perspective = FALSE,
            groups=Groups,
            subpanel = panel.superpose, 
            main = Main,
            screen = Screen2,
            xlim=X.range,ylim=Y.range,zlim=Z.range,
            xlab=Xlab, ylab=Ylab, zlab=Zlab),
            split = c(2,1,2,1))
}
