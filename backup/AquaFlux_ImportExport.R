



#####################################################################
#####################################################################
#             Import-Export Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for data:
#  import-export
#  error checking
#  converting raw units to standard AquaFlux
#  general data wrangling
#  set up
#  data restoration

# Nothing related to UI, plotting, or hardcore computations


##############################################
#   Load previous set up  ##
##############################################


.fill.this.vector = function(y,z){
  n = length(z)
  if (n>0){ y[1:n] = z }
  y
}

.load.site.metadata= function(v){
  
  v$save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
  print("lilo setwd .finish.importing 4")
  
  setwd(v$save.dir)
  x = read.csv(v$tag.site.matadata, stringsAsFactors = F)
  ################ one.liners
  # met
  v$save.dir = .pull.this.vector(x$save.dir)
  v$final.dir = .pull.this.vector(x$final.dir)
  v$graph.dir = .pull.this.vector(x$graph.dir)
  # met & units -changes
  v$met.dir = .pull.this.vector(x$met.dir )
  v$dT.units = .pull.this.vector(x$dT.units) # works
  v$met.air.temp.label = .pull.this.vector(x$met.air.temp.label )#, crash
  v$met.air.temp.units = .pull.this.vector(x$met.air.temp.units)
  v$met.RH.label = .pull.this.vector(x$met.RH.label)
  v$met.RH.units = .pull.this.vector(x$met.RH.units)
  # data structure
  v$max.gap.length = .pull.this.vector(x$max.gap.length )
  v$min.number.of.columns.in.a.data.file = .pull.this.vector(x$min.number.of.columns.in.a.data.file)
  v$delim.sep = .pull.this.vector(x$delim.sep)
  v$number.of.before.headers = .pull.this.vector(x$number.of.before.headers)
  v$number.of.lines.before.data = .pull.this.vector(x$number.of.lines.before.data)
  # time data -  changes
  v$study.year = .pull.this.vector(x$study.year)
  v$selected.timestamp.format = .pull.this.vector(x$selected.timestamp.format)
  v$dT.ts.name = .pull.this.vector(x$dT.ts.name )# crash
  v$met.ts.name = .pull.this.vector(x$met.ts.name) # crash
  # mulit
  v$nonsapflux.columns = .pull.this.vector(x$nonsapflux.columns)
  v$site.names = .pull.this.vector(x$site.names)
  v$site.directories = .pull.this.vector(x$site.directories)
  # calib
  v$alpha =  118.99 * 10^(-6)
  v$beta = 1.231
  
  # Export
  v
}

.pull.this.vector=function(x){
  x = x[is.na(x)==F]
  x
}

.setup.fix.AquaFlux.work.dir = function(a){
  # do you end in "\\"?
  x = substr(a, nchar(a)-1,nchar(a))
  if (x=="\\"){ a = substr(a, 1,nchar(a)-2) } # cut off the last two char
  # do you end in "/"?
  x = substr(a, nchar(a),nchar(a))
  if (x=="/"){ a = substr(a, 1,nchar(a)-1) } # cut off the last char
  # export
  a
}

.save.site.metadata= function(v){
  # ####### Define & create the directory where you want your in-process data to be saved:
  # do you have a "/"
  v$AquaFlux.work.dir = .setup.fix.AquaFlux.work.dir(v$AquaFlux.work.dir)
  # Define the directory where you AquaFlux to save your data:
  v$save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
  # Define the directory where you want your final data to be saved:
  v$final.dir = paste(v$AquaFlux.work.dir, "/Final Data", sep="")
  # Define the directory where you want your final data plots to be stored.
  v$graph.dir = paste(v$AquaFlux.work.dir, "/Graphs", sep="")
  dir.create(v$save.dir, showWarnings = FALSE)
  dir.create(v$final.dir, showWarnings = FALSE)
  dir.create(v$graph.dir, showWarnings = FALSE)
  one.liners = data.frame(
    save.dir = v$save.dir,
    final.dir = v$final.dir,
    graph.dir = v$graph.dir,
    # met & units -changes
    met.dir = v$met.dir,
    dT.units = v$dT.units, # works
    met.air.temp.label = v$met.air.temp.label, #, crash
    met.air.temp.units = v$met.air.temp.units ,
    met.RH.label = v$met.RH.label,
    met.RH.units = v$met.RH.units,
    # data structure
    max.gap.length = v$max.gap.length ,
    min.number.of.columns.in.a.data.file = v$min.number.of.columns.in.a.data.file,
    delim.sep = v$delim.sep,
    number.of.before.headers = v$number.of.before.headers,
    number.of.lines.before.data = v$number.of.lines.before.data,
    # time data -  changes
    study.year = v$study.year,
    selected.timestamp.format = v$selected.timestamp.format,
    dT.ts.name = v$dT.ts.name, # crash
    met.ts.name = v$met.ts.name # crash
  )
  # find max number of lines
  x = c(length(v$nonsapflux.columns), length(v$site.names),
        length(v$site.directories))
  max.x = max(x)
  ###### make things that right length
  # 1 lines
  y = data.frame( matrix(NA, nrow= max.x - 1, ncol=ncol(one.liners)) )
  names(y) = names(one.liners)
  one.liners.mat = rbind( one.liners, y)
  # multi
  y = rep(NA, times = max.x)
  nonsapflux.columns = .fill.this.vector(y,z=v$nonsapflux.columns)
  site.names = .fill.this.vector(y,z=v$site.names)
  site.directories = .fill.this.vector(y,z=v$site.directories)
  #### combine
  z = data.frame(
    nonsapflux.columns =nonsapflux.columns,
    site.names=site.names,
    site.directories=site.directories
  )
  site.metadata = cbind(one.liners.mat,z)
  # save it
  setwd(v$save.dir)
  print("lilo setwd .finish.importing 5")
  
  write.csv(site.metadata,file=v$tag.site.matadata, row.names=F)
}




#####################################################################
# Save  data
#####################################################################


.save.one.file = function( data.to.save, tag, is.Tmax,v){
  study.year=v$study.year
  PDate=v$PDate
  save.dir=v$save.dir
  number.of.saves.to.keep=v$number.of.saves.to.keep
  
  # save it
  if (is.Tmax==T){
    d.clean = data.to.save
  } else{
    d.clean = cbind(PDate,data.to.save)
  }
  name1 = paste( study.year, tag, Sys.time()  )
  names(d.clean)
  name1 = gsub(":", ".", name1)
  write.csv(d.clean,   paste( name1, ".csv", sep="" ), row.names=F )
  
  # clean dir
  jj = paste(study.year, tag); .clean.save.dir(jj,study.year,PDate,save.dir,number.of.saves.to.keep)
}

.clean.save.dir = function(jj,study.year,PDate,save.dir,number.of.saves.to.keep){
  file.list = list.files(save.dir,recursive=T,jj)
  file.list = sort(file.list, decreasing=T)
  if ( length(file.list) > number.of.saves.to.keep ){
    delete.file.list = file.list[ (number.of.saves.to.keep+1) : (length(file.list))]
    for ( n in delete.file.list){ file.remove(n) }
  }
}

.save.AquaFlux= function(v){
  # save files
  print("lilo setwd .finish.importing 6")
  
  setwd(v$save.dir)
  .save.one.file(v$dT.data, v$tag.dT.clean,F,v)
  .save.one.file(v$Tmax.data, v$tag.Tmax, T,v)
  time.last.save = Sys.time()
  #.save.one.file(sapflux.data, .tag.flux, F)  # Not going to bother saving this since it's constantly re-calced.
  #if (exists("flag.data")==T){ .save.one.file(flag.data, .tag.flag, F) }
  # save log
  time.last.save
}

.auto.save.check = function(v){ # this will auto-save your work every 5 turns
  current.time = Sys.time()
  time.diff = difftime( current.time, v$time.last.save, units=c("mins"))
  if ( time.diff > 5){
    time.last.save = .save.AquaFlux(v)
  } else {
    time.last.save = v$time.last.save
  }
  time.last.save
}




#####################################################################
# Set Up
#####################################################################

##############################################
#       Set up functions - step 1         ##
##############################################

.setup1.finish.errorCheck = function(v){
  v$output.message = "The above looks good.  Next questions...";
  v$import.status = "ReadyForNew2"
  # check that file paths are valid
  x = try( setwd(v$AquaFlux.work.dir) ,silent=T); if (class(x)=="try-error"){
    output.message = "ERROR: invalid saving file path"
    v$import.status = "ReadyForNew1"
  }
  x = try( setwd(v$met.dir) ,silent=T); if (class(x)=="try-error"){ 
    output.message = "ERROR: invalid meteorological data file path"
    v$import.status = "ReadyForNew1"
  }
  for (i in v$site.directories){
    x = try( setwd(i) ,silent=T);
    if (class(x)=="try-error"){ 
      output.message = paste("ERROR: invalid data file path:",i) 
      v$import.status = "ReadyForNew1"
    }
  }
  # number of dirs and names the same
  if ( length(v$site.names)!=length(v$site.directories) ) { 
    output.message = "ERROR: number of site names and number of directories not equal" 
    v$import.status = "ReadyForNew1"
  }
  # export
  v
}

.setup1.finish = function(v){
  # check these were entered
  v  = .setup1.finish.errorCheck(v)
  if (v$output.message =="The above looks good.  Next questions..." ){
    v = .read.in.raw.data(v)
  }
  v
}


.brute.combine= function(jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep,min.number.of.columns.in.a.data.file){
  paste("lilo setwd .brute.combine 1")
  file.list = list.files( recursive=T)
  #j=0; j.max = length(file.list); pb <- txtProgressBar(min = 0, max = j.max, style = 3) # for progress bar
  
  k = file.list[1]
  paste("lilo setwd .brute.combine 2")
  
  for (k in file.list){
    file.to.import <<- k;
   # j=j+1; setTxtProgressBar(pb, j) # update progress bar
    x=read.delim(k,sep=delim.sep,
                 stringsAsFactor=F,header=F,
                 skip=number.of.lines.before.data,
                 na.strings = c("NA","NAN") )
    # bigger than min file columns?
    paste("lilo setwd .brute.combine 3")
    
    if (dim(x)[2]>min.number.of.columns.in.a.data.file ){
      
      # name them
      if (  .start.a.data.file == 1 ) { # if you have not yet started a file
        names(x)=names.dx # name it
      } else {
        # get the names
        number.of.before.headers <<- number.of.before.headers
        
        d.x=read.delim(k,sep=delim.sep,
                       stringsAsFactor=F,header=T,
                       skip=number.of.before.headers ,
                       na.strings = c("NA","NAN") )
        head(d.x);
        # save the names
        names(x)=names(d.x)
        names.dx=names(d.x)
      }
      
      # if merge them
      if (  .start.a.data.file == 1 ) { # if you have not yet started a file
        d= rbind(d,x)
      } else {
        d = x;
        .start.a.data.file=1
      }
      
    }
  }
  paste("lilo setwd .brute.combine 4")
  
  file.to.import
  
  #close(pb)
  d <<- d
  
  if (exists(file.to.import)==T){rm(file.to.import)}
}

.combine.site.data = function(wd,jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep, min.number.of.columns.in.a.data.file){
  ######################
  ###### combine site data: combines all the files from one site into one master file
  print("lilo setwd .finish.importing 7")
  print(paste("lilo setwd .finish.importing 7", wd))
  
  setwd(wd);
  print("lilo setwd .finish.importing 7a")
  # import all the files and combine to one gaint thing
  .brute.combine(jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep, min.number.of.columns.in.a.data.file)
  print("lilo setwd .finish.importing 7b")
  
  # clean basics
  dim(d)
  d$RECORD<- NULL
  d= d[ is.na(d$TIMESTAMP)==F, ] # missing time stamp
  d= d[ duplicated(d)==F, ]; dim(d) # delete obvious duplicates
  d=d[ , is.na(names(d))==F ]
  # export
  d
}

.import.met.data = function(v){
  # intialize
  print("lilo setwd .finish.importing 8")
  
  setwd(v$met.dir)
  wd = v$met.dir
  .start.a.data.file <<- 0
  
  #### Actually read in the data
  print("lilo setwd .import.met.data 1")
  
  met.data = .combine.site.data(wd,jj=1,sn="",
                                v$number.of.lines.before.data,
                                v$number.of.before.headers,
                                v$delim.sep,
                                v$min.number.of.columns.in.a.data.file)
  print("lilo setwd .import.met.data 2")
  
  # clean up
  rm(file.to.import,envir = .GlobalEnv)
  
  # export
  met.data
}

.import.raw.dT.data = function(v){
  print("lilo setwd .import.raw.dT.data 1")
  ####### Handle raw data: this command combines ALL RAW data and makes it pretty
  start.a.data.file <<- 0
  # get data from each site and combine them into a dataframe named "d.merge"
  jj=1 # length(site.names)  1:length(site.names)
  for (jj in 1:length(v$site.names) ) {
    # get this site's names
    sd = v$site.directories[jj]
    wd=sd
    sn= v$site.names[jj]
    print("lilo setwd .import.raw.dT.data 2")
    
    # pull in that site's data
    d = .combine.site.data(sd,jj,sn,
                           v$number.of.lines.before.data,
                           v$number.of.before.headers,
                           v$delim.sep,
                           v$min.number.of.columns.in.a.data.file)
    print("lilo setwd .import.raw.dT.data 3")
    
    dim(d);
    
    new.names = paste( sn ,names(d),sep="_") # re-name to include site name
    name.x =  paste( sn ,"TIMESTAMP",sep="_")
    new.names[new.names==name.x] = "TIMESTAMP"
    xx = d
    names(xx)= new.names
    print("lilo setwd .import.raw.dT.data 5")
    
    # merge it
    if (jj==1){ d.merge=xx; }
    if (jj>1){
      d.merge$TIMESTAMP=as.character(d.merge$TIMESTAMP)
      xx$TIMESTAMP=as.character(xx$TIMESTAMP)
      d.merge=merge(x=d.merge,y=xx,all=T,by="TIMESTAMP")
      x = d.merge
    }
    print("lilo setwd .import.raw.dT.data 5")
    
  }
  
  d.merge= d.merge[ duplicated(d.merge$TIMESTAMP)==F, ]
  d.merge
}

.read.in.raw.data = function(v){
  v$raw.met.data = .import.met.data(v)
  v$raw.dT.data =  .import.raw.dT.data(v)
  # export
  v$n.met.data = names(v$raw.met.data)
  v$n.dT.data = names(v$raw.dT.data)
  v
}


##############################################
#       Set up functions - step 2          ##  Mal interrpt
##############################################



#######################################################
# calc sapflux

.get.local.data = function(v){
  if (is.null(v$tree.name)==F){  # v$tree.name!="none" & is.na(v$tree.name)==F
    # time and tree
    tree.number = match(v$tree.name,names(v$dT.data))
    v$tree.number = tree.number
    cc.time = v$LDate>=v$min.DOY & v$LDate<=v$max.DOY;
    v$cc.time = cc.time
    # essentail ts's
    v$LDate.local = v$LDate[cc.time]
    v$dT.local = v$dT.data[cc.time,tree.number]
  }
  # export
  v
}







#####################################################################
# Restore data
#####################################################################

# back up the restores...
#######################################
# data restoration functions

.excute.restore = function(v,backup){
  # needs: tree.number, cc.time, dT.local
  # change local
  v$tree.number = backup$tree.number
  v$cc.time = backup$cc.time
  v$LDate.local = v$LDate[v$cc.time]
  v$dT.local = backup$dT.local
  # change global
  v$dT.data[v$cc.time,v$tree.number] = v$dT.local
  # change button back
  v$qaqc.manual = "none"
  v$restore.option= "none"
  v
}
.restore.undo = function(v){
  if (length(v$backup$dT.local)>0){
    backup = v$backup
    v = .excute.restore(v,backup)
    v$restore.option= "none"
    # erase backup
    v$backup = NULL
  } 
  v
}
.restore.all = function(v){
  # backup filtered data
  v$backup= .qaqc.backup(v)
  # make the backup list
  backup = list()
  backup$tree.number = v$tree.number
  backup$cc.time = v$cc.time
  backup$dT.local = v$raw.data[ v$cc.time, v$tree.number]
  # excute
  v = .excute.restore(v,backup)
  v
}
.restore.between = function(v,thres){
  if (is.null(v$bx)==T) {  # if you don't have any between.points saved
    v$bx = thres$x # save it
  } else { # if you have 1 point saved, and now two
    # get range
    b.range = sort(c( v$bx ,thres$x) ) # save the range
    v$bx = NULL # null it out
    # back up  -- can't do with multipe (I need to right the restore function)
    ###  v$backup= .qaqc.backup(v)
    ######### make backup item -- tree.number
    backup = list()
    backup$tree.number = v$tree.number
    ######### cc.time
    # cc.time: the whole window
    backup$cc.time = v$cc.time
    # cc.time.sec: the restore section
    cc.time.sec = v$LDate >= b.range[1] & v$LDate <= b.range[2]
    ######## dT data
    # the filtered
    backup$dT.local = v$dT.data[ v$cc.time, v$tree.number]
    # plug in the restored part (get from raw data)
    backup$dT.local[cc.time.sec] = v$raw.data[ cc.time.sec, v$tree.number]
    # excute
    v = .excute.restore(v,backup)
  }
  v
}





#####################################################################
# Import
#####################################################################

#################


#######################################################
# the master function: finish importing
#######################################################
.finish.importing = function(v){
  print("lilo setwd .finish.importing 1")
  setwd(v$save.dir)
  
  # get the raw data and it's time vectors
  v = .polish.raw.import(v)
  print("lilo setwd .finish.importing 2")
  
  setwd(v$save.dir)
  
  # get small stuff
  v = .import.small.data(v)
  
  # merge old and new
  v = .merge.old.and.new.dT.data(v)
  # export
  v
}


#######################################################
# raw import
#######################################################
.handle.raw.time.import = function(v){
  #### make it the right format and only get 1 year
  v$raw.met.data = .pull.out.1.year(d=v$raw.met.data, v, this.name=v$met.ts.name)
  v$raw.dT.data = .pull.out.1.year(d=v$raw.dT.data, v, this.name=v$dT.ts.name)
  # make time vectors
  v$raw.met.data = .make.PDate(v$raw.met.data)
  v$raw.dT.data = .make.PDate(v$raw.dT.data)
  ## align time & clean up
  v$raw.met.data  = .align.met.data(v$raw.met.data, v$raw.dT.data, v)
  v = .color.timestamp(v)
  v
}

.check.if.read.in.raw = function(v){
  if (exists("v$raw.met.data")==F){ v$raw.met.data = .import.met.data(v);  }
  if (exists("v$raw.dT.data")==F){ v$raw.dT.data  = .import.raw.dT.data(v); }
  # v$raw.dT.data
  v
} # checked

#######################################################
# load smaller files
.import.small.data = function(v){
  # handle Tmax
  v= .import.get.Tmax(v)
  # handle log.deletion
  v= .get.log.deletion(v)
  # handle flag data
  #v= .get.flag.data(v)
  # export
  v
}


.polish.raw.import = function(v){
  # This function handles your raw data.
  # No data-data taken from v (besides metadata)
  
  ### make sure you have raw data
  v = .check.if.read.in.raw(v)
  v$raw.dT.data$JDate = NULL
  #### make time vectors: LDate, PDate, time.col.  Make sure it's 1 year, properly formated
  v = .handle.raw.time.import(v)
  # get sapflux and nonsapflux i's and names
  v = .get.sapflux.columns(v )
  ## handle units and calc VPD
  v$raw.dT.data = .convert.dT.to.C(v$raw.dT.data,v, v$sapflux.col.i)
  v$raw.met.data = .calc.VPD(v$raw.met.data,v)
  ### interpolate met data & export
  v$met.data = v$raw.met.data
  v$raw.met.data = NULL
  v$met.data = .interpolate.met.data(v$met.data,v)
  # get local
  v$tree.number = v$sapflux.col.i[1]
  # v = .get.local.data(v)
  # export: has time vectors, dT and met upgraded
  v
}

#######################################################
# merge old and new
.merge.old.and.new.dT.data = function(v){
  # read in previous data
  v$dT.data.previous = .read.in.previous.save(v$tag.dT.clean, keep.PDate=T,v)
  v$raw.dT.previous = .read.in.previous.save(v$tag.dT.raw, keep.PDate=T,v)
  does.previous.data.exist = length(v$dT.data.previous)!=1
  
  # if it exists, merge it
  if (does.previous.data.exist==F){
    v = .import.clean.fresh.import(v)
  } else {
    v = .actually.merge.old.and.new.dT.data(v)
  }
  
  v = .import.finish.merger(v)
  v
}

.import.clean.fresh.import = function(v){
  # clean dT and do time
  dT.data = v$raw.dT.data
  
  # dT.data = .polish.previous.import(dT.data,v)
  dT.data$PDate <- NULL
  dT.data$JDate <- NULL
  dT.data$LDate <- NULL
  dT.data$TIMESTAMP <- NULL
  
  # 8) save dT
  v$raw.data  =  dT.data
  v$dT.data = dT.data
  
  # delete
  v$dT.data.previous = NULL
  v$raw.data.previous = NULL
  v$raw.dT.data = NULL
  #  if (do.flags==T){ flag.data  <<- resized.flags }
  # export
  v
  
}

.import.finish.merger = function(v){
  
  # 8.5) Update min.DOY and max.DOY
  v = .update.doy.slider(v)
  v$min.DOY.global = floor(min(v$LDate))
  v$max.DOY.global = ceiling(max(v$LDate))
  v$min.DOY = v$min.DOY.global
  v$max.DOY = v$max.DOY.global
  v$sapflux.names = v$sapflux.names[ v$sapflux.names!="JDate"]
  v$dT.data$JDate = NULL
  v$raw.data$JDate = NULL
  
  # 9) calc sapflux
  # v = .calc.all.sapflux(v) # this does access all the data multiple times and takes a second
  
  # 10) save it
  v$time.last.save = .save.AquaFlux(v)
  .save.one.file(v$raw.data, v$tag.dT.raw,F,v)
  
  # 10.5) number of points in a day (on average)
  median.time.diff = median(diff(v$LDate),na.rm=T) # in partial doy
  v$measurements.in.a.day = round( 1 / median.time.diff )
  
  
  # 11) export
  v
}



.actually.merge.old.and.new.dT.data = function(v){
  # 1) Handle new data
  dT.data = v$dT.data
  raw.data = v$raw.dT.data
  PDate = v$PDate
  raw.data$TIMESTAMP = NULL
  raw.data$JDate = NULL
  updated.dT = raw.data
  
  # 2) Handle previous data
  dT.data.previous = v$dT.data.previous
  raw.dT.previous = v$raw.dT.previous
  dT.data.previous = .polish.previous.import(dT.data.previous,v)
  dT.data.previous$JDate = NULL
  PDate.previous = dT.data.previous$PDate
  dT.data.previous$PDate <- NULL
  dT.data.previous$TIMESTAMP <- NULL
  raw.dT.previous$PDate <- NULL
  raw.dT.previous$TIMESTAMP <- NULL
  
  # 3) resave dT.data.previous to have the same size as raw.data
  x = matrix( nrow=nrow(raw.data), ncol=ncol(raw.data))
  x = as.data.frame(x)
  names(x) = names(raw.data)
  resized.dT.data.previous = x # raw.data*NA # blank slate
  resized.raw.data.previous = x # raw.data*NA # blank slate
  row.matches = match(PDate.previous,PDate ) # match row indexs
  resized.dT.data.previous[row.matches,] = dT.data.previous
  resized.raw.data.previous[row.matches,] = raw.dT.previous
  
  # 4) set up progress bar to merge previous and the new raw
  j=0; j.max = length(names(dT.data.previous)) # for progress bar
 # pb <- txtProgressBar(min = 0, max = j.max, style = 3) # for progress bar
  
  # 5) actually do the merger
  for ( n in names(raw.dT.previous) ) {
   # j=j+1; setTxtProgressBar(pb, j) # update progress bar
    cc = n==names(raw.data)
    # if you have previous data
    if (sum(cc)>0){
      
      # 6)  Determine which times had raw data previously, but are NA in the
      was.deleted = is.na(resized.dT.data.previous[,cc] )==T &  is.na(resized.raw.data.previous[,cc] )==F
      
      # 7) Delete those times from updated.dT
      updated.dT[was.deleted,cc] = NA
    }
  }
  
  # 8) clean things up
  v$dT.data = updated.dT
  v$raw.data  =  raw.data
  v$dT.data.previous = NULL
  v$raw.data.previous = NULL
  #  if (do.flags==T){ flag.data  <<- resized.flags }
  
  # export
  v
}

.polish.previous.import = function(dT.data.previous,v){
  dT.data.previous$TIMESTAMP = dT.data.previous$PDate
  dT.data.previous = .convert.TIMESTAMP(dT.data.previous,v)
  dT.data.previous = .make.PDate(dT.data.previous)
  dT.data.previous$TIMESTAMP <- NULL
  dT.data.previous$JDate <- NULL
  dT.data.previous$LDate <- NULL
  dT.data.previous
}

.update.doy.slider = function(v){
  v$min.DOY.global = floor(min(v$LDate))
  v$max.DOY.global = ceiling(max(v$LDate))
  v$min.DOY = v$min.DOY.global
  v$max.DOY = v$max.DOY.global
  v
}

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
  sapflux.i = v$sapflux.col.i
  dT.units = v$dT.units
  if (dT.units=="C"){  dT.data = dT.data}
  if (dT.units=="K"){  dT.data[,sapflux.i]  = dT.data[,sapflux.i] - 273.15}
  if (dT.units=="F"){  dT.data[,sapflux.i]  = (dT.data[,sapflux.i] - 32) * 5/9 }
  if (dT.units=="mV"){ dT.data[,sapflux.i]  = .convert.mv.to.C( mV=dT.data[,sapflux.i] ) }
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





#####################################################################
# Export
#####################################################################

#############################################################
##             Final Exports Funcitons                    ###
#############################################################
# export to R
# unclick the botton

.export.sapflux = function(v){
  sapflux.data = v$dT.data * NA
  for (tree.name in v$sapflux.names){
    v$tree.name = tree.name
    v$tree.number = match(tree.name,names(sapflux.data))
    v = .get.local.data(v)
    y = .sapflux.calc.local(v,tree.name)
    sapflux.data[,v$tree.number] = y
  }
  tree.name = "SF_Good_data1"
  tree.number = match(tree.name,names(sapflux.data))
  x= sapflux.data[,tree.number]
  # make blank sheet
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, sapflux.data)
  # export
  .save.one.file( z, "Sap flux data- Exported", is.Tmax=F, v)
  sapflux.data <<- z
  sapflux.data
}
.export.Tmax = function(Tmax.data,LDate, sapflux.names, dT.data, v){
  if (nrow(Tmax.data)>1){
    Tmax.data = Tmax.data[is.na(Tmax.data$Name)==F,]
    ########## points
    # make it in the right order
    right.order = order(Tmax.data$Name,Tmax.data$LDate)
    Tmax.data = Tmax.data[right.order,]
    # export points
    .save.one.file( Tmax.data, "Tmax points- Exported", is.Tmax=T, v)
    Tmax.data.points <<- Tmax.data
    #########
    # make line data
    Tmax.data.line = dT.data * NA
    # get lines
    for (tree.name in v$sapflux.names){
      tree.number = match(tree.name,names(v$dT.data))
      Tmax.data.local = .Tmax.get.data(tree.name, Tmax.data, LDate)
      Tmax.data.line[,tree.number] = Tmax.data.local$line
    }
    # export lines
    .save.one.file( Tmax.data.line, "Tmax baseline- Exported", is.Tmax=F, v)
    Tmax.data.line <<- Tmax.data.line
  } else {
    Tmax.data.line = NA
  }
  # kick out Tmax.data.line
  Tmax.data.line
}
.export.raw = function(v){
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, v$raw.data)
  .save.one.file( z, "dT raw- Exported", is.Tmax=F, v)
  raw.dT.data <<- z
}
.export.met = function(v){
  .save.one.file( v$met.data, "Met data- Exported", is.Tmax=F, v)
  met.data <<- v$met.data
}
.export.dT = function(v){
  y = v$met.data
  z = data.frame(TIMESTAMP=y$TIMESTAMP,LDate=y$LDate, v$dT.data)
  .save.one.file( z,  "dT cleaned- Exported", is.Tmax=F, v)
  dT.data <<- z
}
.export.graphs= function(v,sapflux.data,Tmax.data.line){
  for (tree.name in v$sapflux.names){
    # header stuff
    tree.number = match(tree.name,names(v$dT.data))
    v$tree.number = tree.number
    v$tree.name = names(v$dT.data)[v$tree.number]
    # get data
    v = .get.local.data(v)
    # start pdf
    pdf.name = paste(v$study.year,"_",tree.name,".pdf",sep="")
    pdf(pdf.name)
    par(mfrow=c(2,1))
    #####actually plot dT
    v$pick.plot.options = c("Tmax","VPD")
    .plot.dT(v)
    ###### plot sapflux
    v$pick.plot.options = c("flux")
    .plot.sapflux(v)
    
    # .plot.sapflux(v)
    dev.off()
  }
}
.export.final.data = function(v){
  # normal save
  v$min.DOY = v$min.DOY.global
  v$max.DOY = v$max.DOY.global
  v$time.last.save = .save.AquaFlux(v)
  ################### export each data set to csv and base R
  setwd(v$final.dir)
  sapflux.data = .export.sapflux(v)
  Tmax.data.line = .export.Tmax(v$Tmax.data,v$LDate, v$sapflux.names, v$dT.data, v)
  .export.raw(v)
  .export.dT(v)
  .export.met(v)
  ######### export graphs
  setwd(v$graph.dir)
  .export.graphs(v,sapflux.data,Tmax.data.line)
  print("Data exported")
  # report
  #.export.message(v$final.dir,v$graph.dir)
}
