# This function calculates the angle between observer position and monkeys (used in get_monkey_pos function to calculate
# mu = direction of monkey movement which is opposite to observer position)


get_heading = function(lon1, lat1, lon2 = NULL, lat2 = NULL){

  # objects

  # lon1 = cam.pos[1]
  # lat1 = cam.pos[2]
  # lon2 = monkeys.pos[j,1]
  # lat2 = monkeys.pos[j,2]


    dl <- lon2 - lon1
    dla = lat2 - lat1

    return(atan2(dl,dla))

}
