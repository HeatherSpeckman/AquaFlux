
#####################################################################
#####################################################################
#             QAQC / Selection Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for QAQC AquaFlux data.  
# It does not direct UI stuff like the shiny::fluidpage
# or basic plotting functions


#####################################################################
# Man. QAQC
#####################################################################

############################################
# common qaqc functions
.qaqc.excute = function(v,delete.cc){
  # did you actually pick points to delete?
  n.to.delete = sum(delete.cc)
  if (n.to.delete>0){ # if you picked points
    # if (v$qaqc.1orAll=="all"){
    #   # backup
    #   v$backup = .qaqc.backup(v)
    #   # impliment changes
    #   # change local
    #   v$dT.local[delete.cc] = NA
    #   # change global
    #   #  match stuff indexes:   GNAR
    #   nonsapflux.i = match(v$sapflux.names,names(v$dT.data) ) # match col indexs
    #   # to deleete index
    #   times.to.delete = v$LDate.local[delete.cc]
    #   rows.to.delete = match(times.to.delete,v$LDate ) # match row indexs
    #   # actually delete
    #   v$dT.data[rows.to.delete,nonsapflux.i] = NA
    # } else {
    #### Apply to 1 tree
    # backup
    v$backup = .qaqc.backup(v)
    # impliment changes
    # change local
    v$dT.local[delete.cc] = NA
    # change global
    v$dT.data[v$cc.time,v$tree.number] = v$dT.local
    # }
    # change button back
    v$qaqc.manual = "none"
  }
  # export
  v
}
.qaqc.backup = function(v){
  backup=list()
  backup$cc.time = v$cc.time
  backup$tree.number = v$tree.number
  backup$dT.local = v$dT.local
  backup
}

############################################
# individual buttons
.delete.above= function(v,thres){
  # which indexes to delete?
  not.na = is.na(v$dT.local)==F
  out.thres = v$dT.local > thres
  delete.cc = not.na==T & out.thres==T
  # excute
  v = .qaqc.excute(v,delete.cc)
  v
}
.delete.below= function(v,thres){
  # which indexes to delete?
  not.na = is.na(v$dT.local)==F
  out.thres = v$dT.local < thres
  delete.cc = not.na==T & out.thres==T
  # excute
  v = .qaqc.excute(v,delete.cc)
  v
}
.delete.before= function(v,thres){
  # which indexes to delete?
  not.na = is.na(v$dT.local)==F
  out.thres = v$LDate.local < thres
  delete.cc = not.na==T & out.thres==T
  # excute
  v = .qaqc.excute(v,delete.cc)
  v
}
.delete.after= function(v,thres){
  # which indexes to delete?
  not.na = is.na(v$dT.local)==F
  out.thres = v$LDate.local > thres
  delete.cc = not.na==T & out.thres==T
  # excute
  v = .qaqc.excute(v,delete.cc)
  v
}
.delete.between = function(v,thres){
  if (is.null(v$bx)==T) {  # if you don't have any between.points saved
    v$bx = thres # save it
  } else { # if you have 1 point saved, and now two
    b.range = sort(c( v$bx ,thres) ) # save the range
    v$bx = NULL # null it out
    # use
    not.na = is.na(v$dT.local)==F
    in.thres = v$LDate.local >= b.range[1] & v$LDate.local <= b.range[2]
    delete.cc = not.na==T & in.thres==T
    # excute
    v = .qaqc.excute(v,delete.cc)
  }
  v
}
.delete.points= function(v,coordinfo){
  # need to find the nearest point
  not.na = is.na(v$dT.local)==F
  df = data.frame(x=v$LDate.local[not.na],y=v$dT.local[not.na]) # make a dataframe
  nearest.point = nearPoints(df, coordinfo, addDist =T,  maxpoints=1, xvar="x",yvar="y") # this funciton is restricted to only pick 1point at a time
  delete.cc=match(nearest.point$x, v$LDate.local) # find the index
  # excute
  v = .qaqc.excute(v,delete.cc)
  # reset it to points -- this one is unique that it doesn't go back to "none", allowing you to click many times
  v$qaqc.manual="points"
  v
}
.delete.all= function(v){
  # which indexes to delete?
  delete.cc = is.na(v$dT.local)==F
  # excute
  v = .qaqc.excute(v,delete.cc)
  v
}
