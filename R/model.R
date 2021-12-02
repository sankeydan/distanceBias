
# final model function

# define model parameters
model = function (
  
  grid.size.x = NULL ,#set size of grid to 100 squares
  grid.size.y = 5000,
  monk_density_grp_km_2 = NULL, #set number of monkeys
  step.length = NULL, #how many steps from start to finish (number of steps)
  plot_monk = F,  #plot at the end: yes or no
  plot_video_slides = F , #output plots saved to selected folder
  video.name = NULL, #name of ouput file
  scale.param = 30 ,#human scale param
  shape.param = 3,#affects the shape of the detection function
  fid.param = NULL ,#distance at which monkeys run away (from my data). Can use vector of numbers or single value!
  monk.scale.param = NULL, #affects how quickly probability of detection falls
  monk.shape.param = 3, #affects the shape of the detection function
  kappa. = 50, #how narrow is the von mises distribution (higher kappa, narrower the distribution)
  speed.dist.rate.decay = 0.10, #monkey speed: param which gave monkeys a max speed of 100 m/timestep
  norm.mean = 60, # monkey speed when detected
  norm.sd = 15, # sd of speed above
  max.detect.dist = 300,
  di = 1000
  
)
{
  
  ## Variables
  
  # plot_video_slides = F
  # di = 1000
  # max.detect.dist = 600
  # scale.param = 28
  # shape.param = 3
  # speed.dist.rate.decay = 0.15
  #
  # monk_density_grp_km_2 =  monk_density_grp_km_2  # change only these variables
  # fid.param = fid.param
  # grid.size.x = grid.size.x
  # step.length = step.length
  # monk.scale.param = monk.scale.param
  # monk.shape.param = monk.shape.param
  # norm.mean = 60
  # kappa. = 50
  # norm.sd = 15
  
  
  
  #######
  
  # SCRIPT
  
  #######
  
  # Density = num monkey groups
  num.monkey.grps = round( monk_density_grp_km_2 * ((grid.size.x/1000) * (grid.size.y /1000)))
  
  # Video
  if( plot_video_slides){
    video.folder = file.path( PROJHOME, "Figures" , video.name) # PROJHOME is a code found in the main folder (monkeyMAD). To use in a new project just transfer the file ".Rprofile" to main directory of a new project
    if( !file.exists(video.folder) ){
      dir.create(video.folder)
    }
    pdf( file.path( video.folder, paste0(video.name,".pdf") ))
  }
  
  # Cami's trajectory
  camwalk = camwalk.funct(step.length = step.length, grid.size.x = grid.size.x , grid.size.y = grid.size.y, di = di )
  camwalk = rbind( c(0,0),camwalk)
  start.new.transect = c(0,which( diff(camwalk[,"x"]) != 0))+1
  numsteps = (grid.size.y - di*2 )/step.length
  
  
  {
    #make a loop: for each row (nrow) in camwalk dataframe - repeat, and save detected
    detected = matrix( NA, nrow = nrow(camwalk) , ncol = num.monkey.grps) # make empty matrix save detections from obs
    perpendicular = vector()
    transect.num = vector()
    
    #Starting position of monkeys; x and y = sample from a uniform distribution (runif) from 0 (min) to grid.size (max)
    monkeys.pos = cbind( x = runif(num.monkey.grps, 0,grid.size.x),
                         y = runif(num.monkey.grps, 0,grid.size.y))
  }
  
  # loop
  xy=xylims(camwalk[2:nrow(camwalk),"x"],camwalk[2:nrow(camwalk),"y"])
  plot( camwalk,
        xlim = xy[[1]],
        ylim = xy[[2]])
  points ( monkeys.pos)
  points ( camwalk[2:nrow(camwalk),])
  rect  ( xleft = 0, ybottom = 0, xright = grid.size.x, ytop= grid.size.y)
  no.monkeys.on = which ( !duplicated( camwalk[,1]))
  for ( i in 2: nrow ( camwalk ) ){
    #i=2
    
    # cami's position
    if( !identical(camwalk[i,1],camwalk[i-1,1])){
      monkey.sub =which ( monkeys.pos[,1] < (camwalk[i,1]+max.detect.dist) &
                            monkeys.pos[,1] > (camwalk[i,1]-max.detect.dist))
    }
    cam.pos = camwalk[i,]
    
    # distance to each monkey
    mcd = mon.cam.dist( cam.pos,monkeys.pos[monkey.sub,])
    #mcd[mcd>max.detect.dist ] = NA
    
    # does monkey detect cam
    monkdetects.sub = monk.detect.funct(mcd, monk.scale.param = monk.scale.param,monk.shape.param = monk.shape.param)
    monkdetects = rep(0,nrow(monkeys.pos))
    monkdetects[monkey.sub] = monkdetects.sub
    
    # is monkey detected by cam
    detected.sub = c( detect.funct(mcd,scale.param = scale.param,shape.param = shape.param) )
    detected = rep(0,nrow(monkeys.pos))
    detected[monkey.sub] = detected.sub
    #  perpendicular distance to transect. the ones in matrix detected that are = 1,
    #  calculate perpendicular distance and store in perpendicular matrix
    whi =  which( detected == 1)
    perp = perp.funct( camwalk[i,] , monkeys.pos )
    if ( length (whi) > 0  &  ! i  %in% no.monkeys.on ){ # not on first step of new transect. Monkeys haven't had a chance to respond
      perpendicular = c( perpendicular , perp[whi])
      transect.num = c( transect.num , rep ( ceiling( i/numsteps) ,length(whi) ))
    }
    
    # move the monkeys!
    mcds = rep(0,nrow(monkeys.pos))
    mcds[monkey.sub] = mcd
    mcd = mcds
    monkeys.pos.list = get_monkeys_pos( monkeys.pos = monkeys.pos,
                                        cam.pos = cam.pos,
                                        speed.dist.rate.decay = speed.dist.rate.decay,
                                        grid.size.x = grid.size.x,
                                        grid.size.y = grid.size.y,
                                        kappa = kappa.,
                                        monkdetects = monkdetects,
                                        mcd = mcd,
                                        fid.param = fid.param,
                                        norm.mean=norm.mean,
                                        norm.sd = norm.sd)
    dx = monkeys.pos.list[[2]]
    dy = monkeys.pos.list[[3]]
    monkeys.pos = monkeys.pos.list[[1]]
    
    # Kill monkeys! ( Only joking, remove them once spotted)
    monkeys.pos[whi,] = NA
    wh = which (! complete.cases(monkeys.pos))
    if ( length(wh)>0){
      monkeys.pos = monkeys.pos[which ( complete.cases(monkeys.pos)),]
      while( length(wh)>0){
        monkey.sub[monkey.sub>wh[1]] =   monkey.sub[monkey.sub>wh[1]] - 1
        if( length(which( monkey.sub %in% wh[1])) >0){
          monkey.sub = monkey.sub[-which( monkey.sub %in% wh[1])]
        }
        wh = wh[-1]
      }
      if(any(monkey.sub>nrow(monkeys.pos))){
        monkey.sub = monkey.sub[-which(monkey.sub>nrow(monkeys))]
      }
    }
    
    # print
    # print(i)
    
    
  }
  
  # Video
  if( plot_video_slides){
    dev.off() # sometimes needs to be run twice
  }
  
  #### DATA OUTPUT ######
  
  # Make a dataframe of encounters and perpendicular distances (perpend once removed 3 consecutive obs same individual)
  # All units need to be the same (i.e. all m or all km)
  effort = (grid.size.y - di*2 - step.length)/1000
  dat = data.frame ( distance = as.numeric(as.vector( perpendicular))/1000,  # converting distances to km
                     Area = (grid.size.x/1000) * (grid.size.y/1000),  # size of the region of interest (for density estimate) in km
                     Region.Label = "Gola",  # Stratum containing the transect (in this case all in the same region)
                     Effort =  effort, #Lenght of transects (depends on size of grid)
                     Sample.Label = transect.num)  #  ID of the transect (repeat from 1 to however many transects)
  # Give dataframe with complete cases (i.e. remove all NAs)
  dat = dat[complete.cases(dat),]
  numtransects = grid.size.x / di/2
  # Add the transects that are not present in Sample.Label (i.e. the ones with no detections) to dat dataset to account for effort
  whi = which( ! 1:numtransects %in% unique(dat$Sample.Label) )
  for ( i in whi){
    dat = rbind(dat , c( distance = NA,
                         Area = (grid.size.x/1000) * (grid.size.y/1000),
                         Region.Label = "Gola",
                         Effort =  effort ,
                         Sample.Label = i))
  }
  # fix dataset for bug which happens when there are no detections
  if( !any( !is.na(dat[,1] ))){
    dat = as.data.frame(cbind( distance = NA,
                               Area = (grid.size.x/1000) * (grid.size.y/1000),
                               Region.Label = "Gola",
                               Effort = effort ,
                               Sample.Label = 1:numtransects))
  }
  
  # return
  return(dat)
  
  
}
