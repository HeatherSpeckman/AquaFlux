



#####################################################################
#####################################################################
#             Import-Export Functions                               ## 
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



#####################################################################
# Set Up
#####################################################################

##############################################
#       Set up functions - step 1         ##
##############################################

.setup1.finish.errorCheck = function(v){

  v$output.message = "The above looks good.  Next questions...";
  v$import.status = "ReadyForNew2"

  ########### check that file paths are valid
  x = try( setwd(v$AquaFlux.work.dir) ,silent=T); if (class(x)=="try-error"){
    output.message = "ERROR: invalid saving file path"
    v$import.status = "ReadyForNew1"
  }
  x = try( setwd(v$met.dir) ,silent=T); if (class(x)=="try-error"){
    output.message = "ERROR: invalid meteorological data file path"
    v$import.status = "ReadyForNew1"
  }
  for (i in v$dt.dir){
    x = try( setwd(i) ,silent=T);
    if (class(x)=="try-error"){
      output.message = paste("ERROR: invalid data file path:",i)
      v$import.status = "ReadyForNew1"
    }
  }
  ##############################
  # number of dirs and names the same
  if ( length(v$site.names)!=length(v$dt.dir) ) {
    output.message = "ERROR: number of site names and number of directories not equal"
    v$import.status = "ReadyForNew1"
  }

  ###################### look at the headers for each folder
  # dt
  dirx = v$dt.dir[1]
 # .import.test.headers.match(v, dirx)
  # met
  dirx = v$met.dir[1]
#  .import.test.headers.match(v, dirx)

  # export
  v
}


.import.test.headers.match = function(v, dirx){

  file.list = list.files(path=dirx, full.names = T)
  # grab the header
  file.name0 = file.list[1]
  h0=read.delim(file= file.name0,
                nrow=1,
                sep=v$delim.sep,
                stringsAsFactor=F,header=F,
                skip=v$number.of.before.headers,
                na.strings = c("NA","NAN") )

  
  for (file.name in file.list){
    h1=read.delim(file=file.name ,
                  nrow=1,
                  sep=v$delim.sep,
                  stringsAsFactor=F,header=F,
                  skip=v$number.of.before.headers,
                  na.strings = c("NA","NAN") )
    ### test same length, stop if wrong.
    # Developers note: this test was put in to improve stablity, but will restirct function when it comes
    # to having more sensors added to a project.  Place for code improvement
    if (length(h0)!=length(h1) ){
        print("FATAL ERROR: the following two files have different file headers.  File formatting should be consitant")
        print(file.name0)
        print(file.name)
        stop("FATAL ERROR, see above")
    }

    ###### headers match
    a = sum(h0!=h1)
    if (a>0){
      print("FATAL ERROR: the following two files have different file headers.  File formatting should be consitant")
      print(file.name0)
      print(file.name)
      stop("FATAL ERROR, see above")
    }
  }
  ##
}

.setup1.finish = function(v){
  # check these were entered
  v  = .setup1.finish.errorCheck(v)
  if (v$output.message =="The above looks good.  Next questions..." ){
    v = .read.in.raw.data(v)
  }
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
# Import
#####################################################################

#################


#######################################################
# the master function: finish importing
#######################################################
.finish.importing = function(v){
  setwd(v$save.dir)
  # get the raw data and it's time vectors
  v = .polish.raw.import(v)
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
  if (exists("v$raw.dT.data")==F){ 
    #africa
    print(".check.if.read.in.raw  v$raw.dT.data 1")
    print(head(v$raw.dT.data))
    
    v$raw.dT.data  = .import.raw.dT.data(v); }
  
      #africa
    print(".check.if.read.in.raw  v$raw.dT.data 2")
    print(head(v$raw.dT.data))

  
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
  #africa
  print(".polish.raw.import 1")
  print(head(v$raw.dT.data))
  
  # This function handles your raw data.
  # No data-data taken from v (besides metadata)

  ### make sure you have raw data
  v = .check.if.read.in.raw(v)
  #africa
  print(".polish.raw.import 2")
  print(head(v$raw.dT.data))
 
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
