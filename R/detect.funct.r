
# This function specifies whether monkeys is detected or not
# input= distance between cami and monkey, model specifications (curve - probability of detection at different distances)
# output= whether detected or not

detect.funct = function( mcd, scale.param, shape.param){

  # objects

  # hazard rate model = underlying model for the detection process
  y = 1 -exp(-(mcd/scale.param)^-shape.param)
  y[which(is.na(y))] = 0
  # detect?
  #given x number of observations
  #(length of distances between obs and monkey - i.e. each "encounter")---> needs to be perp distance??,
  #how many times does that interaction happen? (only once),
  #and given y function curve
  #is the monkey detected?
  detect = rbinom( length(mcd), 1,y)

  #return
  return(detect)

}
