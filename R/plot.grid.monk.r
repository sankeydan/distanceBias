

plot.grid.monk = function ( cam , monks , cam.radius , grid.size.x = 100 ,
                            grid.size.y= 100, detected.4.plot, monkeydetects, i = NULL) {

  # This function plots the grid, the observer and the monkeys and writes DETECTED if any == 1
  # input = position of observer and monkeys, radius around observer, size of the grid and whether any detected
  # output = plot with observer as black dot and monkeys as red dots

  #define what each one is: cam points will be camwalk positions, monkey points will be monkey.pos...

  # objects
  # cam = camwalk[i,]
  # monks = monkeys.pos
  # cam.radius = cam.rad
  # grid.size = 100
  # detected.4.plot = detected[i,]
  # monkeydetects = monkdetects

  # vars
  g10 = (grid.size.x/10)
  #plot empty grid
  plot( 1,2 , type = "n",
        ylim = c(0,grid.size.y),
        xlim = c(0,grid.size.x),
        ylab = "y", xlab = "x", main = i)

  #add points
  points( cam[1],cam[2], pch= 19)
  points( monks , col = "brown")
  #plotrix::draw.circle( cam[1] , cam[2] , cam.rad)

  if( any( na.omit(  detected.4.plot ) == 1)){
    legend( g10,g10, legend =   "cami saw a monkey!!" , bty = "n", text.col="blue")
  }

  if( any( monkeydetects == 1)){
    legend ( (grid.size.x-g10),g10, legend = "SEEN!", bty = "n", text.col="red")
  }

}
