

#####################################################################
#####################################################################
#             Computational Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for internal computations.
# No ui interactions, no import-exports.


#####################################################################
# Calculate Sapflux
#####################################################################


.calc.sapflux.formula = function(dT,Tmax,v){
  # From Lu 2004, equation 7
  # actually calc the actual fluxes
  flux = v$alpha* (  (Tmax-dT)/(dT)  ) ^ (v$beta)
  # negative fluxes -- force to zero
  flux[ is.na(flux)==F & flux<0] = 0
  # flux currently in m3/m2/s, I want to convert to  g/m2/s
  # conversion:
  # 1m3 = (100)^3cm3  = 1g   -> 1 100^3  1g -> 100^3 g
  # 1   = (1)^3m3     = 1cm3 -> 1    1   1  ->

  flux.g.m2.s = flux * 100^3
  # export
  flux.g.m2.s
}
.sapflux.calc.local = function(v,tree.name){
  # get Tmax data
  Tmax.data.local = .Tmax.get.data(tree.name, v$Tmax.data, v$LDate.local)
  # calc flux
  flux = .calc.sapflux.formula(dT = v$dT.local, Tmax = Tmax.data.local$line, v)
  not.na = sum(is.na(flux)==F)
  flux[flux> 9999 | flux< -9999 ] = NA
  # export
  flux
}


#####################################################################
# Tmax
#####################################################################

###############################
# Tmax functions
# the header     names(Tmax.data) = c("Name","LDate","Tmax")

.Tmax.get.points = function(tree.name, Tmax.data){
  # get Tmax string
  cc = Tmax.data$Name==tree.name & is.na(Tmax.data$Name)==F
  i.Tmax.points = Tmax.data[ cc,]
  i.Tmax.points
}
.Tmax.calc.line = function(i.Tmax.points, LDate.local, have.Tmax){
  # if you have data
  if (have.Tmax==T & nrow(i.Tmax.points)>=2){
    # get dT and LDate data
    Tmax.line = approx( x=i.Tmax.points$LDate , y=i.Tmax.points$Tmax,
                        xout=LDate.local,
                        method="linear",rule=2)
    Tmax.line = Tmax.line$y
  } else {
    Tmax.line = NA
  }
  # export
  Tmax.line
}
.Tmax.get.data = function(tree.name, Tmax.data, LDate.local){
  # get points
  i.Tmax.points = .Tmax.get.points(tree.name, Tmax.data)
  # do you have data
  if (nrow(i.Tmax.points)>0){ have.Tmax = T } else { have.Tmax = F }
  # get local points
  if (have.Tmax==T){
    cc = i.Tmax.points$LDate >= min(LDate.local,na.rm=T) &
      i.Tmax.points$LDate <= max(LDate.local,na.rm=T)
    points.local = i.Tmax.points[ cc, ]
  } else {
    points.local = data.frame(LDate = NA, Tmax = NA)
  }
  # get line
  Tmax.line = .Tmax.calc.line(i.Tmax.points, LDate.local,have.Tmax)
  # bind
  Tmax.data.local = list()
  Tmax.data.local$points = points.local
  Tmax.data.local$line = Tmax.line
  Tmax.data.local$have.Tmax = have.Tmax
  # export
  Tmax.data.local
}
.Tmax.pick.manual = function(v,coordinfo){
  # resect other bottons
  v$qaqc.manual="none"
  v$restore.option="none"
  # save the data
  Name = v$tree.name
  LDate = coordinfo$x
  Tmax = coordinfo$y
  this.line = data.frame(Name=Name,LDate=LDate,Tmax=Tmax)
  # bind
  v$Tmax.data = rbind( v$Tmax.data, this.line)
  # export
  v
}

.Tmax.turn.on.plotting = function(v){
  # did you pick the Tmax botton?
  # if you're not plotting Tmax, plot it now
  if ( sum(v$pick.plot.options=="Tmax")==0 ){ v$pick.plot.options = c(v$pick.plot.options,"Tmax") }
  # if you're not plotting VPD, plot it now
  if ( sum(v$pick.plot.options=="VPD")==0 ){ v$pick.plot.options = c(v$pick.plot.options,"VPD") }
  # export
  v$pick.plot.options
}
.Tmax.delete= function(v,coordinfo){ #
  #### need to find the nearest point
  # make a dataframe
  Tmax.data.local = .Tmax.get.points(v$tree.name, v$Tmax.data)
  df = data.frame(x=Tmax.data.local$LDate,y=Tmax.data.local$Tmax)
  nearest.point = shiny::nearPoints(df, coordinfo, addDist =F, threshold = 100,  maxpoints=1, xvar="x",yvar="y") # this funciton is restricted to only pick 1point at a time
  # get Tmax.data data
  Tmax.data = v$Tmax.data
  Tmax.data = Tmax.data[is.na(Tmax.data$LDate)==F,]
  # get index
  cc = Tmax.data$LDate==nearest.point$x & Tmax.data$Name==v$tree.name
  # save changes
  Tmax.data = Tmax.data[!cc,]
  v$Tmax.data = Tmax.data
  # export
  v
}
.Tmax.calc.time.i.needed = function(LDate,  VPD.below.thres.for.x.hr){
  ##### calculate how many indexes I need to look back to satisfy VPD.below.thres.for.x.hr
  # what is your averge timestep, in hrs?
  time.diff.days = diff(LDate) # get the time diff
  time.diff.days = median(time.diff.days,na.rm=T) # get the median
  time.diff.hrs = time.diff.days * 24 # convert to hours
  # how many indexes is that?
  i.need.for.Tmax.auto = VPD.below.thres.for.x.hr / time.diff.hrs
  i.need.for.Tmax.auto = round(i.need.for.Tmax.auto)
  # export
  i.need.for.Tmax.auto
}
.Tamx.autopick.crit3 = function(v,VPD,VPD.in.range){
  # how many indexes is that?
  if ( is.na(v$i.need.for.Tmax.auto)==T){ # if you don't know
    v$i.need.for.Tmax.auto = .Tmax.calc.time.i.needed(v$LDate,  v$VPD.below.thres.for.x.hr)
  }
  # start with zero delay
  criteria.3.happy = VPD.in.range # for time lag 0
  i.to.go = v$i.need.for.Tmax.auto
  previous.VPD = VPD.in.range
  while (i.to.go>0){
    # what was the VPD one time step ago?
    previous.VPD = c(NA,VPD[1:( length(VPD)-1) ] )
    # does this satifsy?
    previous.VPD.in.range = previous.VPD<=v$VPD.thres & is.na(previous.VPD)==F
    # are we still happy?
    criteria.3.happy = criteria.3.happy==T & previous.VPD.in.range==T
    # keep doing this until you run out of spots
    i.to.go = i.to.go - 1
  }
  # export
  criteria.3.happy
}
.Tmax.autopick.qaulify=function(v){
  ####### which indexes qualify to be in the running. Need 3 criteria:
  # criteria 1: hours between 10 PM and 5 AM
  hour = v$LDate.local - floor(v$LDate.local)
  time.in.range = hour <= (6/24) | hour >= (22/24) # must be at night (between 10 and 5 AM
  # criteria 2: VPD <= v$VPD.thres right now
  VPD = v$met.data$VPD[v$cc.time]
  VPD.in.range = VPD<=v$VPD.thres & is.na(VPD)==F
  ## criteria 3: VPD <= v$VPD.thres for the last VPD.below.thres.for.x.hr
  criteria.3.happy = .Tamx.autopick.crit3(v,VPD,VPD.in.range)
  ## all 3 happy
  all.crit.happy = time.in.range==T & VPD.in.range==T & criteria.3.happy==T
  all.crit.happy
}
.Tmax.autopick.localmax = function(i.qualify,dT.local,LDate.local,tree.name,auto.Tmax.window.size){
  # tree.name
  dT.qualify = dT.local[i.qualify]
  LDate.qualify = LDate.local[i.qualify]
  #LDate.floor = floor(LDate.qualify)
  window.size = auto.Tmax.window.size
  max.dT.list = c()
  max.LDate.list = c()
  # doy.seq
  start.DOY = seq(min(LDate.local), max(LDate.local), by=window.size)
  end.DOY = start.DOY+window.size
  # in each window, find the highest date
  for (i in 1:length(start.DOY)){
    # are you in the window?  Find those indexes
    in.window = LDate.qualify>=start.DOY[i] & LDate.qualify<=end.DOY[i]
    if (sum(in.window)>0){
      # get in-window data
      x.dT = dT.qualify[in.window] # qualifying dT's in the window
      x.LDate = LDate.qualify[in.window] # qualifying LDate's in the window
      # find index of the max
      i.max = which(max(x.dT)==x.dT,x.dT)
      # get the max data
      max.dT = x.dT[i.max]
      max.LDate = x.LDate[i.max]
      # save
      max.dT.list = c(max.dT.list, max.dT)
      max.LDate.list = c(max.LDate.list, max.LDate)
    }
  }
  # export
  Tmax.from.auto.pick = data.frame(Name=tree.name,LDate=max.LDate.list,Tmax=max.dT.list)
  Tmax.from.auto.pick
}


.Tmax.autopick= function(v){
  ## which local indexes qualify to be in the running.
  i.qualify = .Tmax.autopick.qaulify(v)
  # find local max
  if (sum(i.qualify)>0){
    Tmax.from.auto.pick = .Tmax.autopick.localmax(i.qualify,v$dT.local,v$LDate.local,v$tree.name,v$auto.Tmax.window.size)
    v$Tmax.data = rbind(v$Tmax.data, Tmax.from.auto.pick)
  }
  # save it
  v$Tmax.data
}




#####################################################################
#  Broken sensors
#####################################################################

########################
# broken sensor functions.
# Uese a lot of the autoQAQC functions

###############################
#  .BrokenSensors  filter #
###############################

# NA
# super low
# super spikey

.BrokenSensors.lowAverage = function(dT.local,MinDT_brokensensor){
  y.med = median(dT.local, na.rm=T)
  if (y.med< MinDT_brokensensor & is.na(y.med)==F ){  sensor.fail = T } else { sensor.fail=F}
  sensor.fail
}
.BrokenSensors.na = function(dT.local){
  y.na = is.na(dT.local)
  if ( sum(y.na) > ( length(dT.local)/2) ) {  sensor.fail = T } else { sensor.fail=F}
  sensor.fail
}
.BrokenSensors.sd = function(dT.local){
  sd.dT.local = sd(dT.local, na.rm=T)
  max.sd = 10
  if ( sd.dT.local > max.sd & is.na(sd.dT.local)==F) {  sensor.fail = T } else { sensor.fail=F}
  sensor.fail
}
.BrokenSensors.mes = function(mes){
  new.mes = c()
  for( i in 1:length(mes)){
    new.mes = c(new.mes, '<br/>',mes[i])
  }
  HTML(new.mes)
}


###############################
# Functions to look in this window   #
###############################
.autoQAQC.getData = function(tree.name, min.DOY, max.DOY,n.dT.data, LDate, dT.data, LastAutoFilter){
  g = list()
  g$tree.name = tree.name
  g$tree.number = match(tree.name,n.dT.data)
  g$min.DOY = min.DOY
  g$max.DOY = max.DOY

  x.cc.time = LDate>=g$min.DOY & LDate<=g$max.DOY;
  g$LDate.local = LDate[x.cc.time]
  g$dT.local = dT.data[x.cc.time,g$tree.number]
  g$have.data = sum( is.na(g$dT.local)==F )>0
  g$found.suggestion = F
  g$LastAutoFilter = LastAutoFilter

  g
}


###############################
#  Master .BrokenSensors #
###############################
.BrokenSensors.detect = function(v){
  # get bounds and list
  min.DOY = v$max.DOY.global - 5
  max.DOY = v$max.DOY.global
  list.fail.na = c()
  list.fail.low = c()
  list.fail.sd = c()

  for (tree.name in v$sapflux.names){
    # get data
    g = .autoQAQC.getData(tree.name, v$min.DOY, v$max.DOY, n.dT.data=names(v$dT.data), v$LDate, v$dT.data, v$LastAutoFilter)
    # test it
    fail.na = .BrokenSensors.na(g$dT.local)
    fail.low = .BrokenSensors.lowAverage(g$dT.local,v$MinDT_brokensensor)
    fail.sd = .BrokenSensors.sd(g$dT.local)
    # save results
    if (fail.na==T){
      list.fail.na = c(list.fail.na,tree.name)
    } else {
      if (fail.low==T){
        list.fail.low = c(list.fail.low,tree.name)
      } else {
        if (fail.sd==T){
          list.fail.sd = c(list.fail.sd,tree.name)
        }
      }
    }
  }
  # mes them
  v$BrokenSensors.mes.na =  .BrokenSensors.mes(list.fail.na)
  v$BrokenSensors.mes.low =  .BrokenSensors.mes(list.fail.low)
  v$BrokenSensors.mes.sd =   .BrokenSensors.mes(list.fail.sd)

  # export
  v
}




