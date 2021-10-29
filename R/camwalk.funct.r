
# This function gives the position of the observer at each time step
# input = number of steps (camwalk.speed)
# output = matrix of positions

camwalk.funct = function ( step.length = 50, grid.size.x = NULL, grid.size.y = NULL, di = 1000, plott =F){

  # objects

  # step.length =50
  # grid.size.x = 12000
  # grid.size.y = 5000
  # di  = 1000
  # plott = T
  # library( monkeyMAD)

  numtransects = grid.size.x / di/2
  if ( numtransects != round ( numtransects)){
    stop ( "number of transects must be integer. Change grid.size.x")
  }
  xvar = seq( di, grid.size.x-di, length.out = numtransects)
  dx = diff( xvar)[1]

  # for x axis rep x variable. each means x number of times (num.steps)
  # for y axis rep a sequence from x variable at time 1 to x variable (numsteps), x number of times (number of transects)
  numsteps = (grid.size.y - di*2 )/step.length

  camwalk = cbind( x = rep( xvar, each =  numsteps),
                   y = rep( seq( di, (grid.size.y- di ),length.out = numsteps),numtransects))
  if(plott){
    xy=xylims(camwalk[,"x"],camwalk[,"y"])
    plot( camwalk,
        xlim = xy[[1]],
        ylim = xy[[2]])
  }
  return(camwalk)
}
