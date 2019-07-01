


#####################################################################
#####################################################################
#             Post-processing Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for post-processing to be used in base R.
# They are not part of shiny AquaFlux itself.


convert.sapflux.time.units = function( sapflux.data, starting.time.units, disired.time.units ){
  # valid time units:
  # s = second
  # min = minute
  # hr = hour
  # day = day

  # check for valid units
  valid.units = c("s","min","hr","day")
  if (sum(valid.units==starting.time.units)!=1){ stop("Invalid starting.time.units")}
  if (sum(valid.units==disired.time.units)!=1){ stop("Invalid disired.time.units")}

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert current units (starting.time.units) to standard (s)
  if (starting.time.units!="s"){
    if (starting.time.units=="min"){ x = x*60 }
    if (starting.time.units=="hr"){  x = x*60*60 }
    if (starting.time.units=="day"){ x = x*60*60*24 }
  }

  # convert standard units (s) to disired (disired.time.units)
  if (disired.time.units=="s"){   x = x}
  if (disired.time.units=="min"){ x = x/60 }
  if (disired.time.units=="hr"){  x = x/60/60 }
  if (disired.time.units=="day"){ x = x/60/60/24 }

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}


#####################################################################
#             Post-processing: gapfill                      ##
#####################################################################

gapfill.sapflux = function(LDate,y, max.gap.size=20) {
  # remove na for now
  cc = !is.na(y)
  LDate.clean =  LDate[cc]
  y.clean =  y[cc]
  # use a spline to fit the entire series
  s=spline(x=LDate.clean, y=y.clean, xout=LDate)
  y.all.filled = s$y
  # decect whcih values are missing in gaps smaller than max.gap.size
  fill.these.i = .gaps.smaller.than.this.size(y,max.gap.size)
  # fill those indexs in
  y[fill.these.i==T] = y.all.filled[fill.these.i==T]
  y[y<0]=0
  # export
  y
}

#####################################################################
#             Post-processing: re-scale                      ##
#####################################################################



convert.sapflux.m2sapwood.to.m2ground = function( sapflux.data, sapwood.per.ground.area ){
  # sapflux.data MUST be in units that include 1/m2sapwood
  # sapwood.per.ground.area MUST be in (cm2 sapwood / m2 ground)
  # (m2 sapwood / hectare ground) == (cm2 sapwood / m2 ground)

  # units check
  # Have: 1/m2sapwood
  # Disire  1/m2 ground
  # Work it: A = sapwood.per.ground.area
  # 1         * 1 m2sapwood      * A cm2sapwood ->
  # m2sapwood * 100^2 cm2sapwood * m2ground     -> 1 * 100^2 * m2ground
  # -> 1 * 1 * A        -> A
  # -> 1*100^2*m2ground -> 100^2

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert
  x = x * ( sapwood.per.ground.area) / (100^2 )

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}


convert.sapflux.gperm2ground.to.mm = function( sapflux.data, sapwood.per.ground.area ){
  # sapflux.data MUST be in units that include 1/m2sapwood
  # sapwood.per.ground.area MUST be in (cm2 sapwood / m2 ground)
  # (m2 sapwood / hectare ground) == (cm2 sapwood / m2 ground)

  # units check
  # given units:  g
  #               m2ground time
  # disired units: mm
  #                time
  # Needed conversion: g   to  mm
  #                    m2        1
  # Work: 1g   * cm3 *   1m2         * 10mm   ->
  #       1cm3 * m2      100^2cm2      1cm    ->
  # -> 10   -> 10    -> 1
  # -> 100^2-> 10000 -> 1000

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert
  x = x * ( sapwood.per.ground.area) / (1000 )

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}

calc.tree.sapwood.area = function(DBH, sapwood.depth){
  BA.total = pi * (DBH/2)^2
  inner.diam = DBH - sapwood.depth*2
  BA.heartwood = pi * (inner.diam/2)^2
  sapwood.area = BA.total-BA.heartwood
  sapwood.area
}








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





radial.trends = function( sapflux1, sapflux2,
                          sensor.depth1, sensor.depth2,
                          DBH, sapwood.depth,
                          reg.type ){

  # check for valid reg.type
  valid.reg.type = c("mean","gaus","linear")
  if (sum(valid.reg.type==reg.type)!=1){ stop("Invalid reg.type")}

  # calculate sapwood ring area and seq of sensor "depths"
  sapwood.ring.area = .calc.sapwood.ring.area( DBH, sapwood.depth)
  sensor.depth.seq = .calc.sensor.depth( DBH, sapwood.depth)

  if (reg.type=="gaus"){
    s.out = .radial.profile.Gaussian( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                      DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }
  if (reg.type=="linear"){
    s.out = .radial.profile.linear( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                    DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }
  if (reg.type=="mean"){
    s.out = .radial.profile.mean( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                  DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }

  # plot it
  .radial.profile.plot(s.out, sensor.depth.seq,sapflux1, sapflux2,
                       sensor.depth1, sensor.depth2)

  # export
  s.out$sapflow.avg.over.time
}



convert.sapflux.vol.units = function( sapflux.data, starting.vol.units, disired.vol.units ){
  # check for valid units
  valid.units = c("g","cm3","m3","L","gal")
  if (sum(valid.units==starting.vol.units)!=1){ stop("Invalid starting.vol.units")}
  if (sum(valid.units==disired.vol.units)!=1){ stop("Invalid disired.vol.units")}


  # remove cm3 as an option (because cm3=g)
  if (starting.vol.units==("cm3")){ starting.vol.units="g"}
  if (disired.vol.units==("cm3")){ disired.vol.units="g"}

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert current units (starting.time.units) to standard (g)
  if (starting.vol.units=="g"){   x = x}
  if (starting.vol.units=="m3"){ x = x*10^(+6) }
  if (starting.vol.units=="L"){  x = x*1000 }
  if (starting.vol.units=="gal"){ x = x*3785.41 }

  # convert standard units (s) to disired (disired.time.units)
  if (disired.vol.units=="g"){   x = x}
  if (disired.vol.units=="m3"){ x = x/10^(+6) }
  if (disired.vol.units=="L"){  x = x/1000 }
  if (disired.vol.units=="gal"){ x = x/3785.41 }

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}
