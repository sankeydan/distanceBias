Density.estimate = function ( dat,plot.detect=F){

####### DETECTION FUNCTION ######
dat$distance = as.numeric(dat$distance)
dat$distance.raw = dat$distance
# dat$distance = as.numeric( dat$distance.raw )/1000
dat$Area = as.numeric(dat$Area)
dat$Effort = as.numeric(dat$Effort)
dat$Sample.Label = as.character( dat$Sample.Label)
dat$Region.Label = as.character( dat$Region.Label)


# Fitting the detection function with truncation at 100 metres
ds1 <- try(ds(dat , truncation = 0.15))
if ( class( ds1) == "try-error"){
  est = NA
  lcl = NA
  ucl = NA
  se = NA
  sample.truncation = NA
  sample.size = length( na.omit ( dat$distance))
} else {
  d2 = summary(ds1)
  est =  d2$dht$individuals$D$Estimate # location of density estimate in summary output
  # location of density estimate confidence limits
  lcl =  d2$dht$individuals$D$lcl
  ucl =  d2$dht$individuals$D$ucl
  # location of density estimate standard error
  se = d2$dht$individuals$D$se
  sample.truncation = d2$dht$individuals$summary$n
  sample.size = length( na.omit ( dat$distance))
}

# plot detection function?
if ( plot.detect){
  try(plot(ds1, main = paste( vars[i,], collapse = "")))
}

return(  list (
         est                  =  est               ,
         lcl                  =  lcl               ,
         ucl                  =  ucl               ,
         se                   =  se                ,
         sample.truncation    =  sample.truncation ,
         sample.size          =  sample.size       ))
}
