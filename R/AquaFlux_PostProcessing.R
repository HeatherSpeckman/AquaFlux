


#####################################################################
#####################################################################
#             Post-processing Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for post-processing to be used in base R.
# They are not part of shiny AquaFlux itself.


#####################################################################
#             Post-processing: radial trends                     ##
#####################################################################

.calc.sapwood.ring.area = function( DBH, sapwood.depth){
  # logic: I'm going to make the tree into a series of 100 rings,
  # and calc the area of each

  # establish ring diameter's
  ring.width = sapwood.depth / 100 # the distance between the inner and out diam
  d.outer = seq( DBH, DBH-sapwood.depth+ring.width, length=100)
  d.inner = d.outer-ring.width

  # calc ring area via ( area.ring = area.outer.circle - area.inner.circle )
  area.outer.circle = pi * (d.outer/2)^2  # area of the outer circle
  area.inner.circle = pi * (d.inner/2)^2  # area of the inner circle
  area.ring = area.outer.circle - area.inner.circle

  # area.ring is currently in cm2.  Convert to m2
  area.ring.m2 = area.ring/(100^2)

  # export
  area.ring.m2
}

.calc.sensor.depth = function( DBH, sapwood.depth){
  # this function calculates the mock sensor depth seq
  # 0 = bark
  # increases as you go deeper into the tree, till max sapwood.depth

  # establish ring diameter's
  ring.width = sapwood.depth / 100 # the distance between the inner and out diam
  sensor.depth.seq = seq( 0, sapwood.depth+ring.width/2, length=100)
  #summary(sensor.depth.seq)

  # export
  sensor.depth.seq
}

.radial.profile.Gaussian = function( sapflux1, sapflux2,
                                     sensor.depth1, sensor.depth2,
                                     DBH, sapwood.depth,
                                     sapwood.ring.area, sensor.depth.seq){
  ######  Make a Gaussian profile

  ##################### Math
  #  Gaus function
  #y =   a * exp( - ((x-b)^2) / (2*c1^2) )
  #c1 controls wider, a is hieght, b is center

  # for this I will fix b=0

  ### simplify equation
  #y =   a * exp( -(x^2) / (2*c1^2) )
  #y =   a * exp( -(x^2)* (2*c1^2)^(-1) )
  #y =   a * exp( -(x^2)* 2^(-1)*c1^2^(-1) )
  #y =   a * exp( -(x^2)* 2^(-1) * c1^(-2) )
  #y =   a * exp( -(x^2)* 0.5 * c1^(-2) )
  #y =   a * exp( -0.5 * (x^2)* c1^(-2) )

  ### solve for a
  #y1 =   a * exp( -0.5 * (x1^2)* c1^(-2) )
  # sub for k1 = -0.5 * (x1^2)* c1^(-2)
  #y1 =   a * exp( k1 )
  #y1 / exp( k1 )=   a

  #### solve for c1
  # y2 =   a * exp( -0.5 * (x2^2)* c1^(-2) )
  # sub for k2 = -0.5 * (x2^2)* c1^(-2)
  # y2 =   a * exp( k2 )
  # sub for a = y1 / exp( k1 )
  # y2 =  ( y1 / exp( k1 )) * exp( k2 )
  # y2 =  ( y1 * exp(k1)^(-1) ) * exp( k2 )
  # y2 =  y1 * exp(k1)^(-1) * exp(k2)
  # y2/y1 = exp(k1)^(-1) * exp(k2)
  # y2/y1 = exp(k2)/exp(k1)
  # y2/y1 = exp(k2 - k1)
  # log(y2/y1) = k2 - k1
  # sub k3 = log(y2/y1)
  # k3 = k2 - k1
  # k3 = [-0.5 * (x2^2)* c1^(-2)] - [-0.5 * (x1^2)* c1^(-2)]
  # k3 = -0.5* {[ (x2^2)* c1^(-2)] - [(x1^2)* c1^(-2)] }
  # k3 = -0.5 * c1^(-2) * {[ (x2^2)] - [(x1^2)] }
  # k3 = -0.5 * c1^(-2) * { x2^2 - x1^2 }
  # sub k4 = x2^2 - x1^2
  # k3 = -0.5 * c1^(-2) * { k4 }
  # k3 = -0.5 * c1^(-2) * k4
  # k3 * c1^(2) = -0.5 * k4
  # c1^(2) = -0.5 * k4 / k3
  # c1 = ( -0.5 * k4 / k3 )^(1/2)

  ######## run math to find coef
  ## declare stuff
  x1 = sensor.depth1
  x2 = sensor.depth2
  y1 = sapflux1
  y2 = sapflux2

  ## plug in
  k3 = log(y2/y1)
  k4 = x2^2 - x1^2
  c1 = ( -0.5 * k4 / k3 )^(1/2)
  k1 = -0.5 * (x1^2)* c1^(-2)
  a = y1 / exp( k1 )

  ####### calc sapflux by time(rows) and depth(col)
  # sapflux.matrix  g/m2sapwood/s
  sapflux.matrix = matrix( nrow=length(sapflux1), ncol=length(sensor.depth.seq))
  # sapflow.matrix  g/s
  sapflow.matrix = sapflux.matrix
  for (i in 1:length(sensor.depth.seq)){
    x = sensor.depth.seq[i] # modeled sensor depth
    y =  a * exp( -(x^2) / (2*c1^2) ) # modeled sap flux (default units g/m2sapwood/s)
    sapflux.matrix[,i] = y
    sapflow.matrix[,i] = y * sapwood.ring.area[i] # g/m2sapwood/s * m2sapwood => g/s
  }

  # calc products and bind them
  sapflux.avg.over.depth = colMeans(sapflux.matrix, na.rm=T)
  sapflow.avg.over.depth = colMeans(sapflow.matrix, na.rm=T)
  sapflow.avg.over.time  = rowMeans(sapflow.matrix, na.rm=T)
  s.out = list(sapflux.avg.over.depth=sapflux.avg.over.depth,
               sapflow.avg.over.depth=sapflow.avg.over.depth,
               sapflow.avg.over.time=sapflow.avg.over.time)
  # export
  s.out
}



.radial.profile.linear = function( sapflux1, sapflux2,
                                   sensor.depth1, sensor.depth2,
                                   DBH, sapwood.depth,
                                   sapwood.ring.area, sensor.depth.seq){
  ######  Make a Linear profile

  ##################### Math
  #  Linear function
  #y =   m * x + b

  ### solve for b
  # y1 - m * x1 =  b

  # solve for m
  #y2 =   m * x2 + b
  #y2 =   m * x2 + [y1 - m * x1 ]
  #y2 =   m * x2 + y1 - m * x1
  #y2 -y1 = m * x2  - m * x1
  #y2 -y1 = m ( x2 - x1 )
  #(y2 -y1) / (x2-x1)  = m

  ######## run math to find coef
  ## declare stuff
  x1 = sensor.depth1
  x2 = sensor.depth2
  y1 = sapflux1
  y2 = sapflux2

  ## plug in
  m = (y2 -y1) / (x2-x1)
  b = y1 - m * x1

  ####### calc sapflux by time(rows) and depth(col)
  # sapflux.matrix  g/m2sapwood/s
  sapflux.matrix = matrix( nrow=length(y1), ncol=length(sensor.depth.seq))
  # sapflow.matrix  g/s
  sapflow.matrix = sapflux.matrix
  for (i in 1:length(sensor.depth.seq)){
    x = sensor.depth.seq[i] # modeled sensor depth
    y = m * x + b # modeled sap flux (default units g/m2sapwood/s)
    sapflux.matrix[,i] = y
    sapflow.matrix[,i] = y * sapwood.ring.area[i] # g/m2sapwood/s * m2sapwood => g/s
  }

  # calc products and bind them
  sapflux.avg.over.depth = colMeans(sapflux.matrix, na.rm=T)
  sapflow.avg.over.depth = colMeans(sapflow.matrix, na.rm=T)
  sapflow.avg.over.time  = rowMeans(sapflow.matrix, na.rm=T)
  s.out = list(sapflux.avg.over.depth=sapflux.avg.over.depth,
               sapflow.avg.over.depth=sapflow.avg.over.depth,
               sapflow.avg.over.time=sapflow.avg.over.time)
  # export
  s.out
}

.radial.profile.mean = function( sapflux1, sapflux2,
                                 sensor.depth1, sensor.depth2,
                                 DBH, sapwood.depth,
                                 sapwood.ring.area, sensor.depth.seq){
  ######## run math to find coef
  ## declare stuff
  x1 = sensor.depth1
  x2 = sensor.depth2
  y1 = sapflux1
  y2 = sapflux2

  ####### calc sapflux by time(rows) and depth(col)
  # sapflux.matrix  g/m2sapwood/s
  sapflux.matrix = matrix( nrow=length(y1), ncol=length(sensor.depth.seq))
  # sapflow.matrix  g/s
  sapflow.matrix = sapflux.matrix
  for (i in 1:length(sensor.depth.seq)){
    x = sensor.depth.seq[i] # modeled sensor depth
    y = rowMeans(cbind(y1,y2),na.rm=T)
    sapflux.matrix[,i] = y
    sapflow.matrix[,i] = y * sapwood.ring.area[i] # g/m2sapwood/s * m2sapwood => g/s
  }

  # calc products and bind them
  sapflux.avg.over.depth = colMeans(sapflux.matrix, na.rm=T)
  sapflow.avg.over.depth = colMeans(sapflow.matrix, na.rm=T)
  sapflow.avg.over.time  = rowMeans(sapflow.matrix, na.rm=T)
  s.out = list(sapflux.avg.over.depth=sapflux.avg.over.depth,
               sapflow.avg.over.depth=sapflow.avg.over.depth,
               sapflow.avg.over.time=sapflow.avg.over.time)

  # export
  s.out
}

.radial.profile.plot = function(s.out, sensor.depth.seq,
                                sapflux1, sapflux2,
                                sensor.depth1, sensor.depth2){
  # mean observed
  s1.mean = mean(sapflux1, na.rm=T)
  s2.mean = mean(sapflux2, na.rm=T)
  flow.mean = mean(s.out$sapflow.avg.over.time, na.rm=T)
  g = signif(flow.mean,3)


  ###### prep for plotting: flux
  par(mfrow=c(1,2) )
  y= cbind(s1.mean,s2.mean, s.out$sapflux.avg.over.depth )
  ylim=range( y, na.rm=T)
  plot(sensor.depth.seq, s.out$sapflux.avg.over.depth, col=NA, ylim=ylim,
       main="Avg Sap FLUX", ylab="Sap FLUX (vol/m2sapwood/time)",
       xlab="Sapwood depth (0=bark)")
  lines(x=c(0,0),y=c(-5,100), lwd=5, col="brown")
  points(x=c(sensor.depth1, sensor.depth2), y=c(s1.mean, s2.mean),
         col="grey",cex=2,pch=19)
  lines(sensor.depth.seq,s.out$sapflux.avg.over.depth,
        lwd=2, col="blue")


  ###### prep for plotting: flow
  plot(sensor.depth.seq, s.out$sapflow.avg.over.depth, col=NA,
       main="Avg Sap FLOW", ylab="Sap FLOW (vol/time)",
       xlab="Sapwood depth (0=bark)")
  lines(x=c(0,0),y=c(-5,100), lwd=5, col="brown")
  lines(sensor.depth.seq,s.out$sapflow.avg.over.depth,
        lwd=2, col="blue")
  legend.lab = c( paste("mean sap flow=",g), "vol/time/tree")
  legend("topright",  bty = "n",
         legend=legend.lab )
}




