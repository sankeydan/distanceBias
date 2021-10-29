

# running the model under different scenarios

{# skip to loop
  
  # housekeeping
  rm(list=ls())
  folderDS(c ( "Output","theoreticalmodels"))
  
  # libraries
  library(monkeyMAD)
  library(circular)
  library(Distance)
  
  # objects - changable
  n.iter = 1000
  grid.size.x = 20000
  grid.size.y = 5000
  density = 4 #  Different densities of monkeys based on literature hunting intensity
  step.length = 50   # distance between steps
  monk.shape.param = 3
  
  # variables
  flee.mean =   c(10 ,60,150 )#mean of the normal distribution for monkey fleeing
  fid.mean =    c(10 ,60,150 )
  scale    =    c(10,60,150 )
  plot.detect = T
  
  # EXPAND?
  vars = expand.grid(data.frame(flee.mean, fid.mean, scale))       # Create a data frame from all combinations of factor variables
  
  vars.12 = expand.grid(data.frame ( flee.mean,fid.mean))
  vars.13 = expand.grid(data.frame ( flee.mean, scale  ))
  vars.23 = expand.grid(data.frame ( fid.mean , scale  ))
  
  vars.123 = rbind (  cbind( vars.12, scale =   "30"), cbind ( vars.12, scale     = "100"))
  vars.132 = rbind (  cbind( vars.13, fid.mean ="30"), cbind ( vars.13, fid.mean  = "100"))
  vars.231 = rbind (  cbind( vars.23, flee.mean="30"), cbind ( vars.23, flee.mean = "100"))
  
  vars.132 = vars.132[,c(1,3,2)]
  vars.231 = vars.231[,c(3,1,2)]
  
  vars = rbind ( vars,  vars.123, vars.132 , vars.231)
  m=1
}



#################################################################

######################## LOOP #################################

############################################################


for ( i in 1:nrow(vars)){ # For each unique set of variables
  
  #i=1
  
  # Monkey detection function
  monk.scale.param = as.numeric(vars$scale[i])
  
  # FID
  fid.param = rnorm (1000, as.numeric(vars$fid.mean[i]),as.numeric(vars$fid.mean[i])/4)
  
  # density
  monk_density_grp_km_2 = density
  
  
  # fleemean
  norm.mean = as.numeric( vars$flee.mean[i])
  
  # function
  namdat = c("distance"   ,  "Area"   , "Region.Label", "Effort", "Sample.Label")
  
  ######### Simulation
  dat.list = list()
  den.list = list()
  vec= vector()
  n.iter.2 = n.iter # Use this to "try again" if there is an error in the model. Super rare and hard to identify this bug from within the function
  for( j in 1:n.iter.2){ # - FOR EACH ITERATION
    
    ### RUN THE MODEL
    t1 = Sys.time()
    dat = try( model (
      monk_density_grp_km_2 =  monk_density_grp_km_2 , # change only these variables
      fid.param = fid.param,
      grid.size.x = grid.size.x,
      step.length = step.length,
      monk.scale.param = monk.scale.param ,
      monk.shape.param = monk.shape.param ,
      norm.mean = norm.mean,
      norm.sd = norm.mean/4
    ))
    
    if(class(dat)!="try-error"){
      
      # Density estimate
      Den = Density.estimate(dat,plot.detect = T)
      vec.old = vec
      vec = c(vec, mean(c(vec,Den$est),na.rm = T))
      if(any(!is.na(vec))){
        plot(vec)
      }
      
      print( Sys.time() - t1)
      dat.list[[j]] = dat
      den.list[[j]] = Den
      print(paste ( m , "/" , nrow(vars) * n.iter))
      m = m+1
      
    } else{
      n.iter.2 = n.iter.2+1
    }
  }
  
  dat.list2 = list( data = dat.list , density.estimate = den.list,  vars =  vars[i,],density = density)
  fileName =paste0 ( paste(c(as.vector(t(cbind(names(vars),t(vars[i,])))),"density",density),collapse = "_"), ".rda")
  save( dat.list2 , file = file.path(PROJHOME,"Output","theoreticalmodels",fileName))
  
}


