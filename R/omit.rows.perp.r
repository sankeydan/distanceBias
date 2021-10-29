

# This function tells R not to save perpendicular distances of consecutive encounters
# after first one for the next three positions

omit.rows.perp = function(perpendicular,consecutive=3){
  for ( i in 1:ncol(perpendicular)){
    #i=1
    vec = perpendicular[,i]
    cu = 0
    on = F

    for ( j in 1:length(vec)){
      # j =8
      if ( on == F){
        if( !is.na(vec[j])){
          cu = j
          on = T
        }
      }
      if( !is.na(vec[j]) & j-cu >consecutive){
        cu = j
      }
      if (  j -cu > 0.5 & j - cu < (consecutive+0.5) ){
        vec[j] = NA
      }
      if ( j - cu > (consecutive-0.5) ){
        on = F
      }
    }
    perpendicular[,i] = vec
  }
  return(perpendicular)
}
