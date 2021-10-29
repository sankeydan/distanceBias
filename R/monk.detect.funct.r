# This function specifies whether monkeys detect me or not
# input= distance between cami and monkey, model specifications (curve - probability of detection at different distances)
# output= whether detected or not (and thus following on from this wether they run away or not)

monk.detect.funct = function( mcd, monk.scale.param, monk.shape.param){

  # objects

  # mcd = mcd
  # monk.model = NULL
  # detection.rate = detection.rate
  # max.det.distance = max.det.distance
  # monk.scale.param = monk.scale.param
  # monk.shape.param = monk.shape.param


  # hazard rate model = underlying model for the detection process
    y = 1 -exp(-(mcd/monk.scale.param)^-monk.shape.param)
y[which(is.na(y))] = 0


  # detect?
  #given x number of observations
  #(length of distances between obs and monkey - i.e. each "encounter")
  #how many times does that interaction happen? (only once),
  #and given y function curve
  #is the monkey detected?

  monk.detect = rbinom( length(mcd), 1,y)

  #return
  return(monk.detect)

}
