.brute.combine= function(jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep,
                         min.number.of.columns.in.a.data.file, start.a.data.file){
  file.list = list.files( recursive=T)
  print("in .brute.combine 1")
  #j=0; j.max = length(file.list); pb <- txtProgressBar(min = 0, max = j.max, style = 3) # for progress bar
  k = file.list[1]
  file.started = F
  for (k in file.list){
    file.to.import <<- k;
    # j=j+1; setTxtProgressBar(pb, j) # update progress bar
    x=read.delim(k,sep=delim.sep,
                 stringsAsFactor=F,header=F,
                 skip=number.of.lines.before.data,
                 na.strings = c("NA","NAN") )
    dim(x)
    # bigger than min file columns?
    if (dim(x)[2]>min.number.of.columns.in.a.data.file ){
      # name them
      if (  file.started == T ) { 
        names(x)=names.dx # name it
      } else {
        # get the names
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
      if (  file.started == T ) { 
        d= rbind(d,x)
      } else {
        d = x;
        file.started == T
      }

    }
  }
  if (exists(file.to.import)==T){rm(file.to.import)}
    print("in .brute.combine 2")

  ### export 
  d
}

.combine.site.data = function(wd,jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep, 
                              min.number.of.columns.in.a.data.file, start.a.data.file){
  print("in .combine.site.data 1") 
  ######################
  ###### combine site data: combines all the files from one site into one master file
  setwd(wd);
  # import all the files and combine to one gaint thing
  d = .brute.combine(jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep, 
                 min.number.of.columns.in.a.data.file, start.a.data.file)
  # clean basics
  dim(d)
  d$RECORD<- NULL
  # clear null TIMESTAMPS
    print("in .combine.site.data 2") 

  have.TIMESTAMP.col = sum( names(d)=="TIMESTAMP")>0
  if (have.TIMESTAMP.col){
    d= d[ is.na(d$TIMESTAMP)==F, ] # missing time stamp
  }
  d= d[ duplicated(d)==F, ]; dim(d) # delete obvious duplicates
  d=d[ , is.na(names(d))==F ]
    print("in .combine.site.data 3") 

  # export
  d
}

.import.met.data = function(v){
      print("in .import.met.data 1") 

  # intialize
  setwd(v$met.dir)
  wd = v$met.dir
  #### Actually read in the data
  met.data = .combine.site.data(wd,jj=1,sn="",
                                v$number.of.lines.before.data,
                                v$number.of.before.headers,
                                v$delim.sep,
                                v$min.number.of.columns.in.a.data.file,
                               start.a.data.file=0)
  # clean up
  rm(file.to.import,envir = .GlobalEnv)
  # export
  met.data
}

.import.raw.dT.data = function(v){
        print("in .import.raw.dT.data 1") 

  ####### Handle raw data: this command combines ALL RAW data and makes it pretty
  # get data from each site and combine them into a dataframe named "d.merge"
  jj=1 # length(site.names)  1:length(site.names)
  for (jj in 1:length(v$site.names) ) {
    # get this site's names
    sd = v$dt.dir[jj]
    wd=sd
    sn= v$site.names[jj]
    # pull in that site's data
    d = .combine.site.data(sd,jj,sn,
                           v$number.of.lines.before.data,
                           v$number.of.before.headers,
                           v$delim.sep,
                           v$min.number.of.columns.in.a.data.file, 
                          start.a.data.file=0)
    dim(d);
        print("in .import.raw.dT.data 2") 

    new.names = paste( sn ,names(d),sep="_") # re-name to include site name
    name.x =  paste( sn ,"TIMESTAMP",sep="_")
    new.names[new.names==name.x] = "TIMESTAMP"
    xx = d
    names(xx)= new.names
    # merge it
    if (jj==1){ d.merge=xx; }
    if (jj>1){
      d.merge$TIMESTAMP=as.character(d.merge$TIMESTAMP)
      xx$TIMESTAMP=as.character(xx$TIMESTAMP)
      d.merge=merge(x=d.merge,y=xx,all=T,by="TIMESTAMP")
      x = d.merge
    }
  }
          print("in .import.raw.dT.data 3") 
  d.merge= d.merge[ duplicated(d.merge$TIMESTAMP)==F, ]
          print("in .import.raw.dT.data 4") 

  d.merge
}

.read.in.raw.data = function(v){
  print(".read.in.raw.data 1")
  v$raw.met.data = .import.met.data(v)
    print(".read.in.raw.data 2")

  v$raw.dT.data =  .import.raw.dT.data(v)
    print(".read.in.raw.data 3")

  # export
  v$n.met.data = names(v$raw.met.data)
    print(".read.in.raw.data 4")

  v$n.dT.data = names(v$raw.dT.data)
    print(".read.in.raw.data 5")

  v
}
