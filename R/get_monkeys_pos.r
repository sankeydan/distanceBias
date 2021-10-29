library(circular)

# This function gives the movement of the monkeys at each time step
# Direction of movement depends on whether monkeys detects observer and whether he is close enough (FID) to flee
# input = monkey starting position, monkey speed, grid size, direction of movement(mu), narrowness of bell (kappa)
# output = monkey new position at each time step

#add noise?

get_monkeys_pos = function( monkeys.pos ,
                            cam.pos,
                            speed.dist.rate.decay,
                            grid.size.x,
                            grid.size.y,
                            kappa = 1, # default for now
                            monkdetects,
                            mcd,
                            fid.param,
                            norm.mean=60,
                            norm.sd = 10){

  # objects

  # monkeys.pos = monkeys.pos
  # cam.pos = cam.pos
  # speed.dist.rate.decay = speed.dist.rate.decay
  # grid.size.x = grid.size.x
  # grid.size.y = grid.size.y
  # kappa = kappa # default for no
  # monkdetects = monkdetects
  # mcd = mcd
  # fid.param = fid.param
  # pois.const = 10

  # libraries
  library(circular)

  #add in monkey direction and new position
  num.monkeys = nrow(monkeys.pos)

  # empty objects
  dx = rep(NA,num.monkeys)
  dy = rep(NA,num.monkeys)

  for (j in 1:num.monkeys){
    #{
    # j=1

    # direction to cam
    mu = get_heading(cam.pos[1],
                     cam.pos[2],
                     monkeys.pos[j,1],
                     monkeys.pos[j,2])


    ######## If monkey detects cami, MOVE AWAY

    # fid
    sampleWithoutSurprises <- function(x) {
      if (length(x) <= 1) {
        return(x)
      } else {
        return(sample(x,1))
      }
    }
    sampled.fid = sampleWithoutSurprises(fid.param)

    if (monkdetects[j] == 1 & mcd[j] < sampled.fid ){

      monkey.dir = rvonmises (1,circular(mu), kappa, control.circular=list(units="radians"))                 #vonmises distribution
      monkey.dir = ifelse (monkey.dir > pi,monkey.dir -2*pi, monkey.dir)
      monk.speeds = rnorm (1, mean=norm.mean, sd=norm.sd) # normal distribution for fleeing speed

      # plot the distribution ?

      # par ( mfrow = c(2,1))
      # hist( round( rexp( 1000, speed.dist.rate.decay)),breaks = 15, xlab = "Distance (m)" , main = "Distance travelled in six minutes when not detected")
      # hist( rpois(1000, lambda = 60), breaks = 15                , xlab = "Distance (m)" )
      # hist(rnorm(1000,mean=60, sd=10))


     } else {

      ###### otherwise RANDOM WALK

      monkey.dir  = runif(1, -pi , pi) # runif distribution
      monk.speeds = round ( rexp( 1, speed.dist.rate.decay)) #exponetial decay curve

    }

    if ( monkey.dir  > -pi && monkey.dir < (-pi/2)){
      theta = pi +
        monkey.dir
      dx[j] = -abs( monk.speeds * sin(theta) )  # change in x
      dy[j] = -abs( monk.speeds * cos(theta) )  # change in y
    }
    if ( monkey.dir  > (-pi/2) && monkey.dir < 0){
      theta = -pi/2 +
        monkey.dir
      dx[j] = -abs( monk.speeds * cos(theta) )
      dy[j] = abs( monk.speeds * sin(theta) )
    }
    if ( monkey.dir > 0 && monkey.dir < (pi*(1/2)) ){
      theta = 0 +
        monkey.dir
      dx[j] = abs( monk.speeds  * sin(theta) )
      dy[j] = abs( monk.speeds * cos(theta) )
    }
    if ( monkey.dir > (pi*(1/2)) && monkey.dir < (pi) ){
      theta = pi*(1/2) +
        monkey.dir
      dx[j] = abs(monk.speeds  * cos(theta) )
      dy[j] = - abs(monk.speeds * sin(theta) )
    }
    monkeys.pos[j,] = monkeys.pos[j,] + c(dx[j],dy[j])
  }

  for ( j in 1:2){
    #j=2

    if ( j == 1){
      grid.size = grid.size.x
    }
    if ( j == 2){
      grid.size = grid.size.y
    }
    monkeys.pos[,j] =ifelse ( monkeys.pos[,j] > grid.size,grid.size, monkeys.pos[,j] ) # if any monkey position (x and y- i.e. columns) above 100,let it be = 100
    monkeys.pos[,j] =ifelse ( monkeys.pos[,j] < 0        ,  0,       monkeys.pos[,j] ) # if any monkey position (x and y- i.e. columns) below 0, let it be = 0
  }

  monkeys.pos = cbind(monkeys.pos)

  return(list ( monkeys.pos, dx,dy))
}
