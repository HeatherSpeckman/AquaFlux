
#####################################################################
# Import sub-functions  
#####################################################################


#######################################################
# mini functions handle time
#######################################################

.convert.TIMESTAMP = function(d,v){#cow
  ############# actually convert time
  t1 = strptime(d$TIMESTAMP, format=v$standard.time.format, tz="UTC") # convert to time step
  cc = is.na(t1)==T
  t1[cc] =  strptime(d$TIMESTAMP[cc], format=v$alternate.time.format, tz="UTC") # convert to time step
  cc = is.na(t1)==T
  t1[cc] =  strptime(d$TIMESTAMP[cc], format=v$third.time.format, tz="UTC") # convert to time step
  cc = is.na(t1)==T
  #  if (v$selected.timestamp.format!="none"){
  t1[cc] =  strptime(d$TIMESTAMP[cc], format=v$selected.timestamp.format, tz="UTC") # convert to time step
  cc = is.na(t1)==T
  # check if you got them all
  d$TIMESTAMP[ cc ]
  if( sum(is.na(t1)==T)>0 ){
    print( paste( "Fatal error-- Unknown time format in file") ) # keep p statement
    print(d$TIMESTAMP[ cc ])
    stop()
  }
  d$TIMESTAMP = t1
  # export
  d
} # checked

.pull.out.1.year= function(d,v,this.name){
  # rename column to be TIMESTAMP
  d$TIMESTAMP = d[,names(d)==this.name]
  # use strptime
  d = .convert.TIMESTAMP(d,v)
  # filter for only this year
  y = as.numeric(format(d$TIMESTAMP,format="%Y"))
  d = d[y==v$study.year,]
  # remove duplicates
  d= d[ duplicated(d)==F, ]; dim(d) # delete obvious duplicates
  # order
  d = d[order(d$TIMESTAMP),]

  sum(is.na(d$TIMESTAMP))
  # export
  d
} # checked

.make.PDate = function(d){
  PDate = d$TIMESTAMP
  JDate = as.numeric(format(PDate,format="%j"))
  h = as.numeric(format(PDate,format="%H"))
  m = as.numeric(format(PDate,format="%M"))
  hour <- h/24 + m/(24*60) # hour or partail day (0.5 = noon)
  LDate = JDate+hour # linear day
  # export
  d$PDate = PDate
  d$JDate = JDate
  d$LDate = LDate
  d
} # checked

.color.timestamp = function(v){
  print(".color.timestamp 1")#africa
  # export important vector
  raw.dT.data = v$raw.dT.data
  v$LDate = raw.dT.data$LDate
  v$PDate = raw.dT.data$PDate
  # make hour
  h = as.numeric(format(v$PDate,format="%H"))
  m = as.numeric(format(v$PDate,format="%M"))
  hour <- h/24 + m/(24*60) # hour or partail day (0.5 = noon)
  # make the time colors
  time.col=hour*NA
  time.col[ hour<=4/24 ] = "red"
  time.col[ hour>=4/24 & hour<8/24 ] = "orange"
  time.col[ hour>=8/24 & hour<12/24 ] = "green"
  time.col[ hour>=12/24 & hour<16/24 ] = "darkgreen"
  time.col[ hour>=16/24 & hour<20/24 ] = "blue"
  time.col[ hour>=20/24 & hour<24/24 ] = "purple"
  v$time.col = time.col
  # clean up
  raw.dT.data$PDate = NULL
  raw.dT.data$LDate = NULL
  raw.dT.data$raw.dT.data = NULL
  v$raw.dT.data = raw.dT.data
  # export
  v
} # checked


.align.met.data = function(met.data,dT.data,v){
  met.data$LDate = round(met.data$LDate,3)
  met.data$PDate = NULL
  dT.data$LDate.orginal = dT.data$LDate
  dT.data$LDate = round(dT.data$LDate,3)
  dT.data = dT.data[order(dT.data$PDate),]
  # combine
  x = data.frame(TIMESTAMP=dT.data$TIMESTAMP, JDate=dT.data$JDate,
                 LDate=dT.data$LDate, PDate=dT.data$PDate )
  met.sync = merge(x=x, y=met.data, all.x=T,all.y=F)
  ##### clean up
  met.sync$PDate = NULL
  met.sync$LDate.orginal = NULL
  # get rid of sapflow data
  met.sync
} # checked



#######################################################
# convert units
#######################################################

.get.airT.in.C = function(met.data,v){
  ##### get the data
  if (v$met.air.temp.label=="<No Air T data>"){
    # if you have no data, make a vector of NA's
    airT = rep(NA, nrow(met.data))
  } else {
    airT = met.data[, names(met.data)==v$met.air.temp.label ]
  }
  # convert units
  u = v$met.air.temp.units
  if (u=="NA"){  airTC = rep(NA, nrow(met.data))}
  if (u=="C"){ airTC = airT}
  if (u=="K"){ airTC = airT - 273.15; }
  if (u=="F"){ airTC = (airT - 32) * 5/9 }
  # export
  airTC
}



.get.RH.in.percent = function(met.data,v){
  ###### get the data
  if (v$met.RH.label=="<No RH data>"){
    # if you have no data, make a vector of NA's
    RH = rep(NA, nrow(met.data))
  } else {
    # if you have data, grab it
    RH = met.data[, names(met.data)==v$met.RH.label ]
  }
  # convert units
  if (v$met.RH.units=="NA"){  RH.percent = rep(NA, nrow(met.data)) }
  if (v$met.RH.units=="%"){ RH.percent = RH}
  if (v$met.RH.units=="Decimal"){ RH.percent = RH * 100;  }
  # export
  RH.percent
}

.convert.dT.to.C=function(dT.data,v,sapflux.i){
  print("in .convert.dT.to.C 1") #africa
  print(v$sapflux.col.i) #africa
  print(v$dT.units) #africa
  sapflux.i = v$sapflux.col.i
  dT.units = v$dT.units
  if (dT.units=="C"){  dT.data = dT.data}
  if (dT.units=="K"){  dT.data[,sapflux.i]  = dT.data[,sapflux.i] - 273.15}
  if (dT.units=="F"){  dT.data[,sapflux.i]  = (dT.data[,sapflux.i] - 32) * 5/9 }
  if (dT.units=="mV"){ dT.data[,sapflux.i]  = .convert.mv.to.C( mV=dT.data[,sapflux.i] ) }
    print("in .convert.dT.to.C 2") #africa
  print(head(dT.data)) #africa
  # export
  dT.data
}

.convert.mv.to.C = function(mV){
  # https://www.omega.com/techref/pdf/z198-201.pdf
  E = mV * 1000
  c0 = 0
  c1= 2.592800 * 10^(-2)
  c2 = -7.602961 * 10^(-7)
  c3 = 4.637791 * 10^(-11)
  c4 = -2.165394 * 10^(-15)
  c5 = 6.048144 * 10^(-20)
  c6 = -7.293422 * 10^(-25)
  c7 = 0
  C = c0 + c1*E^1 + c2*E^2 + c3*E^3 + c4*E^4 + c5*E^5 + c6*E^6 + c7*E^7
  C
}




#######################################################
# met handling
#######################################################

.calc.VPD = function(met.data,v){
  ##### calc VPD
  AirTC = .get.airT.in.C(met.data,v)
  RH = .get.RH.in.percent(met.data,v)
  SVP = 610.7*10^( (7.5*AirTC) / (237.3+AirTC) ) # units Pa
  SVP = SVP / 1000 # now in kPa
  # source: http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
  VPD = ( (100-RH )/100)*SVP
  met.data$AirTC = AirTC
  met.data$VPD = VPD
  met.data$RH = RH
  met.data
}

.spline.fit.all = function(xout,y){
  cc = is.na(y)==F

  #### if you have data to interpolate
  if (sum(cc)>0){
    y = y[cc]
    x = xout[cc]
    # calc interpolator
    s=spline(x=x, y=y, xout=xout)
    # save it
    y[!cc] = s$y[!cc]
  } else {
    #### if you don't have data to interpolate
  }

  # export
  y
}

.gaps.smaller.than.this.size= function(y,max.gap.length){

  # which i's missing vales
  y.na.i = which(is.na(y))
  # calc the gap length
  if (length(y.na.i)>0){
    gap.counter = rep(0,length(y))
    for(i in y.na.i){
      i.min = i-1; i.min[i.min<1] = 1
      gap.counter[i] = gap.counter[i.min] + 1
    }
    gap.counter
    #
    too.high.i= which(gap.counter>max.gap.length)
    too.high.v= gap.counter[too.high.i]
    too.high.i
    too.high.v
    # fill
    fill.this = gap.counter * 0
    fill.this[y.na.i] = T # these are the gapped indexes
    # declare stuff F for too.high.i times
    if (length(too.high.i)>0){
      for(z in 1:length(too.high.i)){
        i = too.high.i[z]
        v = too.high.v[z]
        i.min = i-v
        fill.this[i.min:i] = F
      }
    }
    y.filled = fill.this
  }  else {
    y.filled = y
  }
  # export
  y.filled
}

.interpolate.met.data = function(met.data,v) {
  # save stuff
  met.data$VPD.raw = met.data$VPD
  met.data$VPD.gapfill = met.data$VPD
  # fill all gaps
  y = met.data$VPD.raw
  xout = v$LDate
  yout = .spline.fit.all(xout,y)
  # which value am I supposed to fill?
  fill.these = .gaps.smaller.than.this.size(y,v$max.gap.length)
  y[fill.these==T] = yout[fill.these==T]
  # save it
  met.data$VPD.gapfill = y
  # export
  met.data
}

#######################################################
# read in previous data
.read.in.previous.save= function(tag,keep.PDate,v){
  file.list = list.files(v$save.dir,recursive=T,paste(v$study.year,tag))
  x = "missing"
  # actually get data
  if (length(file.list)>0){
    file.name = sort(file.list, decreasing=T)[1]
    x = read.csv( file.name, stringsAsFactors = F)
    # handle PDate
    if (keep.PDate==T){ v$PDate.previous = x$PDate }
  }
  # export
  x
}

.import.get.Tmax = function(v){
  # read in old Tmax
  Tmax.data = .read.in.previous.save(v$tag.Tmax, keep.PDate=F,v)
  # if you don't have Tmax data, make it
  if (length(Tmax.data)==1){
    Tmax.data = data.frame(Name=NA, LDate=NA, Tmax=NA)
  }
  v$Tmax.data = Tmax.data # done
  v
}

.get.log.deletion = function(v){
  # read in old
  log.deletion = .read.in.previous.save(v$tag.log.deletion, keep.PDate=F,v)
  # if you don't have Tmax data, make it
  if (length(log.deletion)==1){
    log.deletion = data.frame(tree.name=NA,tree.number=NA,delete.tool.used=NA,threshold=NA,threshold.2=NA, min.DOY=NA,max.DOY=NA)
    log.deletion = log.deletion[is.na(log.deletion$tree.name)==F,]
  }
  v$log.deletion = log.deletion
  v
}

.get.sapflux.columns = function(v ){
  v$n.dT.data = names(v$raw.dT.data)

  s = seq(1,length(v$n.dT.data) )
  nonsapflux.col.i = match(v$nonsapflux.columns, v$n.dT.data )
  v$sapflux.col.i = setdiff(s,nonsapflux.col.i)
  v$sapflux.names = setdiff(v$n.dT.data,v$nonsapflux.columns)  #names(v$n.dT.data)[v$sapflux.col.i]
  v
}




