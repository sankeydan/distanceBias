
# This function calculates the distance between the observer and the monkeys at each time step
# input = positions of observer and monkeys
# output = distances between observer and all monkeys

mon.cam.dist = function (cam , monks){

  # VARIABLES
  # cam = camwalk[i,]
  # monks = monkeys.pos

  # OBJECTS
  num.monkeys = ifelse ( is.null(nrow(monks)),1,nrow(monks))

  # DISTANCE
  #make an empty vector called dist. rep is repeating values in x (in this case NA for empty vector) for number of monkeys
  #empty vector will be filled by the function return
  dist = rep(NA, num.monkeys)

  # at each position of observer (X) and monkeys (Y), calculate the sides (cateti) as difference between x and y positions
  # and then the hypothenuse (i.e. distance between observer and monkey)
  for ( i in 1:num.monkeys){

    X = cam         #camwalk
    if( num.monkeys == 1){ # monkeys
      Y = monks
      } else {
        Y = monks[i,]
      }
    c =  abs( X[1] - Y[1] ) # distance
    d =  abs( X[2] - Y[2] )
    e = sqrt( c^2 + d^2 )

    dist[i] = e
  }

  # return distance
  return( dist )

}
