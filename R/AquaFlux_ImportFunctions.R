
.brute.combine= function(jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep,
                         min.number.of.columns.in.a.data.file, start.a.data.file){ 
  file.list = list.files( recursive=T)
  #j=0; j.max = length(file.list); pb <- txtProgressBar(min = 0, max = j.max, style = 3) # for progress bar
  k = file.list[1]
  file.started = F
  j=1
  for (k in file.list){
    j=j+1
    file.to.import = k;   
    
    #### read in the data
    x=read.delim(k,sep=delim.sep,
                 stringsAsFactor=F,header=F,
                 skip=number.of.lines.before.data,
                 na.strings = c("NA","NAN") )    
    
    # bigger than min file columns? 
    if (dim(x)[2]>min.number.of.columns.in.a.data.file ){
      ###### save the column names and bind together.      
      if (  file.started == T ) { 
        names(x)=names.dx # name it
        d= rbind(d,x)
      } else { 
        ########## get the names
        d.x=read.delim(k,sep=delim.sep,
                       stringsAsFactor=F,header=T,
                       skip=number.of.before.headers ,
                       na.strings = c("NA","NAN") )
        # save the names
        names(x)=names(d.x)
        names.dx=names(d.x)
        
        # bind it
        d <- x;
        file.started <- T
      }  
    }
  }
  if (exists(file.to.import)==T){rm(file.to.import)}

  d
}




.combine.site.data = function(wd,jj,sn,number.of.lines.before.data, number.of.before.headers,delim.sep, 
                              min.number.of.columns.in.a.data.file, start.a.data.file){
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
  have.TIMESTAMP.col = sum( names(d)=="TIMESTAMP")>0
  if (have.TIMESTAMP.col){
    d= d[ is.na(d$TIMESTAMP)==F, ] # missing time stamp
  }
  d= d[ duplicated(d)==F, ]; dim(d) # delete obvious duplicates
  d=d[ , is.na(names(d))==F ]
  # export
  d
}

.import.met.data = function(v){
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
  #rm(file.to.import,envir = .GlobalEnv)
  # export
  met.data
}

.import.raw.dT.data = function(v){ 
  #africa
  print("in .import.raw.dT.data 1")
  print(head(v$raw.dT.dat))
  
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
  
    #africa
  print("in .import.raw.dT.data 3")
  print(head(d.merge))
  
  # deactivated: remove duplicated timestamps
  #d.merge= d.merge[ duplicated(d.merge$TIMESTAMP)==F, ]

  d.merge
}

.read.in.raw.data = function(v){
  v$raw.met.data = .import.met.data(v)
  v$raw.dT.data =  .import.raw.dT.data(v)
  # africa
  print("in .read.in.raw.data 1")
    print("v$raw.dT.data")
  print(v$raw.dT.data)
  # export 
  v$n.met.data = names(v$raw.met.data)
  v$n.dT.data = names(v$raw.dT.data)
  v
}
